{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

import CmdLine (Options(..), getOptions)

import Control.Monad
import Text.Printf
import Network.Socket (withSocketsDo)
import Happstack.Server
import Data.List (isPrefixOf, isSuffixOf, sortOn, isInfixOf, nub)
import Data.Maybe (fromJust, isJust, fromMaybe, mapMaybe, isNothing)
import Data.FileEmbed
import System.IO.Temp
import System.FilePath
import Data.IORef
import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)

import Text.Highlighting.Kate
import Text.Megaparsec.String (Parser)
import Text.Megaparsec as MP
import Text.Blaze.Html5 ((!), toHtml, docTypeHtml, Html, toValue)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import GHC
import Outputable (SDoc,PprStyle,renderWithStyle,showSDoc,ppr)
import GHC.Paths ( libdir )
import FastString (unpackFS)
import DynFlags
import Control.Concurrent
import Control.Concurrent.STM
import System.Directory
import Data.Time.Clock
import Safe
import Extra
import Numeric
import Profiles
import Text.Read (readEither)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Exception (evaluate)


data Compilation = Compilation
   { compilFlags   :: !DynFlags
   , compilSources :: ![File]
   , compilLogs    :: ![Log]
   , compilPhases  :: !PhaseInfo
   }

data Log = Log
   { logId       :: !Word
   , logDynFlags :: !DynFlags
   , logReason   :: !WarnReason
   , logSeverity :: !Severity
   , logLocation :: !(Maybe Location)
   , logStyle    :: !PprStyle
   , logMessage  :: !SDoc
   }

instance Eq Log where
   (==) l1 l2 = logId l1 == logId l2

instance Ord Log where
   compare l1 l2 = compare (logId l1) (logId l2)

instance Show Log where
   show l = "Log "++ show (logId l) ++ ":"
            ++ "\n\tSeverity: "++ showLogSeverity l
            ++ "\n\tContents: "++ showLogMessage l

data Location = Location
   { locFile      :: !String
   , locStartLine :: !Int
   , locEndLine   :: !Int
   , locStartCol  :: !Int
   , locEndCol    :: !Int
   }

type LogParser = Parsec Dec [Log]

instance MP.Stream [Log] where
   type Token [Log] = Log
   uncons []     = Nothing
   uncons (x:xs) = Just (x,xs)
   updatePos _ _ pos@(SourcePos n l c) _  = (pos,apos)
      where
         apos = SourcePos n (unsafePos (unPos l + 1)) c


data PhaseCoreSize = PhaseCoreSize
   { phaseCoreSizeName      :: !String
   , phaseCoreSizeTerms     :: !Word
   , phaseCoreSizeTypes     :: !Word
   , phaseCoreSizeCoercions :: !Word
   } deriving (Show)

data PhaseChildType
   = PhaseRawLog [Log]
   | PhaseCoreSizeLog PhaseCoreSize
   | PhaseDumpLog Block
   | PhaseChild PhaseInfo
   deriving (Show)

data PhaseInfo = PhaseInfo
   { phaseName        :: !String
   , phaseModule      :: !(Maybe String)
   , phaseDuration    :: !Float
   , phaseMemory      :: !Float
   , phaseChildren    :: ![PhaseChildType]
   , phasePath        :: ![Int]
   } deriving (Show)

emptyPhaseInfo :: PhaseInfo
emptyPhaseInfo = PhaseInfo
   { phaseName     = ""
   , phaseModule   = Nothing
   , phaseDuration = 0
   , phaseMemory   = 0
   , phaseChildren = []
   , phasePath     = []
   }
   

data Block = Block
   { blockName     :: String
   , blockDate     :: Maybe UTCTime
   , blockContents :: String
   } deriving (Show)

convertLocation :: SrcSpan -> Maybe Location
convertLocation = \case
   UnhelpfulSpan _ -> Nothing
   RealSrcSpan s   ->
      Just $ Location (prepareFile (unpackFS (srcSpanFile s)))
                      (srcSpanStartLine s) (srcSpanEndLine s)
                      (srcSpanStartCol  s) (srcSpanEndCol  s)
   where
      prepareFile s
         | "./" `isPrefixOf` s = drop 2 s
         | otherwise           = s

data CompState
   = Compiling
   | Parsing
   | Compiled Compilation

main :: IO ()
main = withSocketsDo $ do

   opts <- getOptions

   infiles <- forM (optfiles opts) $ \src ->
               File src <$> readFile src

   comps <- newTVarIO Map.empty
   profs <- newTVarIO (Map.fromList ([0..] `zip` defaultProfiles))

   quit <- newEmptyMVar

   let conf = nullConf {port = optport opts}
   putStrLn (printf "Starting Web server at localhost:%d" (port conf))
   httpTID <- forkIO $ simpleHTTP conf $ msum
      [ dir "css" $ dir "style.css" $ ok css
      , dir "script.js" $ ok js

      -- "Compiling..." page
      , showCompilingMaybe infiles profs comps

      , dir "quit" $ nullDir >> do
         liftIO $ putMVar quit ()
         tempRedirect "/" (toResponse "")

      , nullDir >> do
         ps   <- liftIO $ readTVarIO profs
         cs   <- liftIO $ readTVarIO comps
         html <- showManager infiles ps cs
         ok $ toResponse $ appTemplate html
      ]

   takeMVar quit
   killThread httpTID
   putStrLn "Quit."



phasePhaseChildren :: PhaseInfo -> [PhaseInfo]
phasePhaseChildren = mapMaybe f . phaseChildren
   where
      f (PhaseChild c) = Just c
      f _              = Nothing
                           

data File = File
   { fileName     :: String
   , fileContents :: String
   } deriving (Show)


compileFiles :: [File] -> CompilationProfile -> IO ([Log],DynFlags)
compileFiles files prof = do
   logs <- newIORef []
   cnt  <- newIORef 0

   dflgs <- withSystemTempDirectory "ghc-web" $ \tmpdir -> do
      -- write module files
      forM_ files $ \file -> do
         -- FIXME: check that fileName is not relative (e.g., ../../etc/passwd)
         createDirectoryIfMissing True (tmpdir </> takeDirectory (fileName file))
         writeFile (tmpdir </> fileName file) (fileContents file)

      withCurrentDirectory tmpdir $ do
         -- execute ghc
         runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags

            -- we catch logs generated by GHC
            let logact dfl reason sev srcspan style msg = do
                  lid <- readIORef cnt
                  modifyIORef' logs (Log lid dfl reason sev (convertLocation srcspan) style msg :)
                  modifyIORef' cnt (+1) 
                  --log_action dflags dfl reason sev srcspan style msg

            let dflags' = (profileFlags prof dflags)
                  { dumpDir    = Just tmpdir
                  , log_action = logact
                  }
            void $ setSessionDynFlags dflags'
            target <- guessTarget (fileName (head files)) Nothing
            setTargets [target]
            void $ load LoadAllTargets

            return dflags'

   logs' <- reverse <$> readIORef logs
   return (logs',dflgs)




css :: Response
css = toResponseBS (C.pack "text/css") (L.fromStrict $(embedFile "src/style.css"))

js :: Response
js = toResponseBS (C.pack "text/javascript") (L.fromStrict $(embedFile "src/script.js"))

showHead :: Html -> Html
showHead hdr = do
   H.head $ do
      H.title (toHtml "GHC Studio")
      H.meta ! A.httpEquiv (toValue "Content-Type")
             ! A.content   (toValue "text/html;charset=utf-8")
      H.link ! A.rel       (toValue "stylesheet") 
             ! A.type_     (toValue "text/css")
             ! A.href      (toValue "/css/style.css")
      H.script (return ())
               ! A.type_ (toValue "text/javascript")
               ! A.src (toValue "script.js")
      H.style ! A.type_ (toValue "text/css") $ toHtml $ styleToCss tango
      hdr

showBody :: Html -> Html
showBody bdy = do
   H.body $ do
      H.div (toHtml "GHC Studio")
         ! A.class_ (toValue "headtitle")
      H.div (do
         H.a (toHtml ("Home")) ! A.href (toValue "/")
         toHtml "  -  "
         H.a (toHtml ("Quit")) ! A.href (toValue "/quit")
         ) ! A.class_ (toValue "panel")
      bdy

-- | Template of all pages
appTemplate :: Html -> Html
appTemplate bdy = docTypeHtml $ do
   showHead (return ())
   showBody bdy

showManager :: [File] -> Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showManager files profs comps = do
   fileList <- showSourceList files
   profList <- showProfileList profs comps
   compList <- showCompilePageList profs comps
   (pageList,page) <- showPage files profs comps

   return $ do
      H.table $ H.tr $ do
         H.td (do
               fileList
               profList
               compList
               pageList
            ) ! A.class_ (toValue "left-panel")
         H.td page ! A.class_ (toValue "page")

isNewProfile :: ServerPartT IO Bool
isNewProfile = (== Just "new") <$> optional (lookRead "profile")

getSelectedProfile :: Map Int CompilationProfile -> ServerPartT IO (Maybe (Int,CompilationProfile))
getSelectedProfile profs = do
   selProf <- optional (lookRead "profile")
   case selProf of
      Nothing -> return Nothing
      Just i  -> case Map.lookup i profs of
         Nothing -> return Nothing
         Just p  -> return (Just (i,p))

getSelectedProfileId :: Map Int CompilationProfile -> ServerPartT IO (Maybe Int)
getSelectedProfileId profs = (fmap fst) <$> getSelectedProfile profs

getCompilation :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO (Maybe (Int,Compilation))
getCompilation profs comps = do
   getSelectedProfile profs >>= \case
      Nothing    -> return Nothing
      Just (i,_) -> case Map.lookup i comps of
         Nothing           -> return Nothing
         Just (Compiled c) -> return (Just (i,c))
         Just Compiling    -> mempty
         Just Parsing      -> mempty

lookCompilation :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO (Int,Compilation)
lookCompilation profs comps = getCompilation profs comps >>= \case
   Nothing -> mempty
   Just c  -> return c

showBox :: String -> Html -> Html
showBox title bdy = H.div (do
   H.div (toHtml title) ! A.class_ (toValue "boxTitle")
   H.div bdy ! A.class_ (toValue "boxBody")
   ) ! A.class_ (toValue "box")

-- | Show already compiled profiles
showProfileList :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showProfileList profs comps = do
   selProf <- getSelectedProfileId profs
   newProf <- isNewProfile
   let ps = Map.filterWithKey (\i _ -> Map.member i comps) profs
   let html = showBox "Profiles" $ H.table $ do
         forM_ (Map.toList ps) $ \(i,prof) -> H.tr $ do
            H.td $ H.a (toHtml (profileName prof))
               ! A.href (toValue ("/?profile="++show i))
               ! if Just i /= selProf
                  then mempty
                  else A.class_ (toValue "selectedItem")
         H.td $ H.a (toHtml "* New run *")
               ! A.href (toValue ("/?profile=new"))
               ! if not newProf
                  then mempty
                  else A.class_ (toValue "selectedItem")
   return html

-- | Show list of input files
showSourceList :: [File] -> ServerPartT IO Html
showSourceList files = do
   let html = showBox "Files" $ H.table $ do
         forM_ files $ \file -> H.tr $ do
            H.td $ H.a (toHtml (fileName file))
               ! A.href (toValue ("/?page=source&file="++fileName file))
   return html

uriSelectCompilePage :: Map Int CompilationProfile -> Map Int CompState -> String -> ServerPartT IO H.AttributeValue
uriSelectCompilePage profs comps page = do
   selComp <- getCompilation profs comps
   case selComp of
      Just (p,_) -> return $ toValue $ "/?profile="++show p++"&page="++page
      Nothing    -> mempty

showCompilePageList :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showCompilePageList profs comps = do
   getCompilation profs comps >>= \case
      Nothing -> return (return ())
      Just _  -> do -- TODO: only display pages valid for the given compil
         currentPage <- fromMaybe "" <$> optional (look "page")
         let makeItem title page = do
               uri <- uriSelectCompilePage profs comps page
               return $ H.a (toHtml title)
                  ! A.href uri
                  ! if currentPage /= page
                     then mempty
                     else A.class_ (toValue "selectedItem")

         uriConf         <- makeItem "Configuration" "config"
         uriOverview     <- makeItem "Overview" "overview"
         uriCoreOverview <- makeItem "Core phases overview" "core-overview"
         uriAllPhases    <- makeItem "All phases" "all-phases"
         uriIR           <- makeItem "Intermediate representations" "ir"
         uriRemLog       <- makeItem "Unparsed log" "rem-log"
         uriFullLog      <- makeItem "Full log" "full-log"
      
         return $ showBox "Run infos" $ H.table $ do
            H.tr $ H.td $ uriConf
            H.tr $ H.td $ uriOverview
            H.tr $ H.td $ uriCoreOverview
            H.tr $ H.td $ uriAllPhases
            H.tr $ H.td $ uriIR
            H.tr $ H.td $ uriRemLog
            H.tr $ H.td $ uriFullLog

showPage :: [File] -> Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO (Html,Html)
showPage files profs comps = do
   selPage <- optional (look "page")
   
   case selPage of
      Nothing -> getSelectedProfile profs >>= \case
         Just (_,p) -> (return (),) <$> showProfile p
         Nothing    -> isNewProfile >>= \case
            True  -> (return (),) <$> showProfileListPage profs comps
            False -> (return (),) <$> showDefault files profs comps
      Just "source" -> (return (),) <$> showInputFile files
      Just p  -> getCompilation profs comps >>= \case
         Nothing    -> mempty
         Just (ci,c) -> case p of
            "config"        -> (return (),) <$> return (showDynFlags (compilFlags c))
            "overview"      -> (return (),) <$> showOverview profs comps
            "core-overview" -> showCoreSizeEvolution profs comps
            "all-phases"    -> showPhases profs comps
            "phase"         -> (return (),) <$> showPhase ci c
            "full-log"      -> showLogs files profs comps
            "rem-log"       -> showRemLogs files profs comps
            "ir"            -> showIRs files profs comps
            _               -> (return (),) <$> return (toHtml "TODO")

showDefault :: [File] -> Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showDefault _ _ _ = return $ do
   H.p $ toHtml "GHC studio helps you debug your program and/or GHC."
   H.p $ toHtml "Start a compilation by choosing \"New run\" on the left"

showProfileListPage :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showProfileListPage profs _comps = return $ do
   H.table $ forM_ (Map.toList profs) $ \(i,prof) -> do
      H.tr $ do
         H.td $ H.a (toHtml (profileName prof)) 
            ! A.href (toValue ("/?profile="++show i))
         H.td $ toHtml (profileDesc prof)


showCompilingMaybe :: [File] -> TVar (Map Int CompilationProfile) -> TVar (Map Int CompState) -> ServerPartT IO Response
showCompilingMaybe files profs comps = do
   pid <- lookRead "profile"
   ps <- liftIO $ readTVarIO profs
   case Map.lookup pid ps of
      Nothing   -> mempty
      Just prof -> do
         (needCompile,compilState) <- liftIO $ atomically $ do
            cs <- readTVar comps
            case Map.lookup pid cs of
               -- not found: not compiled
               Nothing -> do
                  modifyTVar comps (Map.insert pid Compiling)
                  return (True,Compiling)
               -- may be still compiling or already compiled
               Just c  -> return (False,c)
         
         when needCompile $ do
            liftIO $ void $ forkIO $ do
               (logs,dflags) <- compileFiles files prof
               atomically $ modifyTVar comps (Map.insert pid Parsing)
               phaseInfos <- evaluate $ makePhaseInfos logs
               let comp = Compilation dflags files logs phaseInfos
               atomically $ modifyTVar comps (Map.insert pid (Compiled comp))

         case compilState of
            Compiled _ -> mempty
            cstate     -> ok $ toResponse $ showCompiling cstate


showProfile :: CompilationProfile -> ServerPartT IO Html
showProfile prof = return $ do
   H.h3 $ toHtml (profileName prof)
   H.p $ toHtml (profileDesc prof)

-- | Template of all pages
showCompiling :: CompState -> Html
showCompiling cstate = docTypeHtml $ do
   showHead $ 
      H.meta ! A.httpEquiv (toValue "refresh")
             ! A.content   (toValue "2")
   showBody $ do
      H.div (do
         toHtml "Compiling..."
         case cstate of
            Parsing -> H.br >> toHtml "Parsing logs..."
            _       -> return ()
         ) ! A.class_ (toValue "compiling")

showInputFile :: [File] -> ServerPartT IO Html
showInputFile files = do
   p' <- look "file"
   let p = if "/" `isPrefixOf` p' then tail p' else p'
   case filter ((==p) . fileName) files of
      []       -> mempty
      (file:_) -> return $ do
         H.h2 (toHtml (fileName file))
         H.div $ toHtml
            $ formatHtmlBlock defaultFormatOpts
            $ highlightAs "haskell" (fileContents file)

showIRs :: [File] -> Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO (Html,Html)
showIRs _ profs comps = do
   (_,comp) <- lookCompilation profs comps
   md       <- optional $ look "module"

   let phases' = phasePhaseChildren (compilPhases comp)
       phases  = case md of
         Nothing -> phases'
         Just _  -> filter ((== md) . phaseModule) phases'

       go phase = do
         forM_ (phaseChildren phase) $ \case
            PhaseDumpLog blk -> showBlock blk
            PhaseChild p     -> go p
            _                -> return ()

   pageList <- showModuleFilter profs comps phases' "ir"
   let page = forM_ phases go
   return (pageList,page)

showLogs :: [File] -> Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO (Html,Html)
showLogs files profs comps = do
   (cid,comp) <- lookCompilation profs comps
   fileFilter <- optional $ look "file"

   let filterHtml = showBox "Filter by file:" $ do
         H.table $ do
            H.tr $ H.td $ H.a (toHtml "All")
               ! A.href (toValue ("/?profile="++show cid++"&page=full-log"))
               ! if fileFilter /= Nothing
                  then mempty
                  else A.class_ (toValue "selectedItem")
            forM_ files $ \file -> do
               H.tr $ H.td $ H.a (toHtml (fileName file))
                  ! A.href (toValue ("/?profile="++show cid++"&page=full-log&file="++fileName file))
                  ! if fileFilter /= Just (fileName file)
                     then mempty
                     else A.class_ (toValue "selectedItem")

   let page = do
         let
            logFilter clog = case (fileFilter,logLocation clog) of
               (Nothing,_)      -> True
               (Just f, Just s) -> locFile s == f
               _                -> False
            logs = filter logFilter (compilLogs comp)

         case fileFilter of
            Nothing -> showLogTable logs
            Just f  -> do
               let
                  pos = locEndLine . fromJust . logLocation
                  ers = sortOn fst
                           $ fmap (\x -> (locEndLine $ fromJust $ logLocation $ head x,x))
                           $ groupOn pos
                           $ sortOn pos
                           $ filter (isJust . logLocation) logs

                  file   = head (filter ((== f) . fileName) (compilSources comp))
                  source = highlightAs "haskell" (fileContents file)

                  go _           []  []               = return ()
                  go currentLine src []               = do
                     let opts = defaultFormatOpts
                           { numberLines = True
                           , startNumber = currentLine+1
                           }
                     formatHtmlBlock opts src
                  go currentLine src ((n,errs):errors)
                     | n == currentLine = showLogTable errs >> go (currentLine+1) src errors
                     | otherwise        = do
                        let (src1,src2) = splitAt (n - currentLine) src
                        let opts = defaultFormatOpts
                              { numberLines = True
                              , startNumber = currentLine+1
                              }
                        formatHtmlBlock opts src1
                        showLogTable errs
                        go n src2 errors

               H.div $ toHtml $ go 0 source ers

   return (filterHtml,page)

showRemLogs :: [File] -> Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO (Html,Html)
showRemLogs _ profs comps = do
   (_,comp) <- lookCompilation profs comps

   let 
      f (PhaseRawLog ls) = ls
      f (PhaseChild c)   = go (phaseChildren c)
      f _                = []

      go ps = concatMap f ps
      logs  = go (phaseChildren (compilPhases comp))
      page  = showLogTable logs

   return (return (),page)

showLogSeverity :: Log -> String
showLogSeverity l = case logSeverity l of
   SevOutput      -> "Output"
   SevFatal       -> "Fatal"
   SevInteractive -> "Interactive"
   SevDump        -> "Dump"
   SevTrace       -> "Trace"
   SevInfo        -> "Info"
   SevWarning     -> "Warning"
   SevError       -> "Error"

showLogMessage :: Log -> String
showLogMessage l = showSDoc (logDynFlags l) (logMessage l)

showLogTable :: [Log] -> Html
showLogTable logs = do
   H.table (do
      H.tr $ do
         H.th (toHtml "Severity")
         H.th (toHtml "Reason")
         H.th (toHtml "Location")
         H.th (toHtml "Message")
      forM_ logs $ \clog -> do
         H.tr $ do
            H.td $ toHtml $ showLogSeverity clog
            H.td (toHtml $ case logReason clog of
               NoReason    -> "-"
               Reason flag -> getWarningFlagName flag)
               ! A.style (toValue ("min-width: 12em"))
            H.td (toHtml $ case logLocation clog of
               Nothing -> "-"
               Just s  -> locFile s
                  ++ " (" ++ show (locStartLine s)
                  ++ "," ++ show (locStartCol  s)
                  ++ ") -> (" ++ show (locEndLine s)
                  ++ "," ++ show (locEndCol  s) ++ ")")
               ! A.style (toValue ("min-width: 12em"))
            H.td (H.pre $ toHtml $
               renderWithStyle (logDynFlags clog) (logMessage clog) (logStyle clog)
               ) ! A.class_ (toValue "logTableMessage")
         ) ! A.class_ (toValue "logtable")


getFlagName :: (Show a, Enum a) => Maybe [FlagSpec a] -> a -> String
getFlagName specs opt = case specs of
      Nothing -> show opt
      Just ss -> case filter ((== fromEnum opt) . fromEnum . flagSpecFlag) ss of
         []    -> show opt
         (x:_) -> flagSpecName x

getWarningFlagName :: WarningFlag -> String
getWarningFlagName = getFlagName (Just wWarningFlags)

showDynFlags :: DynFlags -> Html
showDynFlags dflags = do
   let showFlagEnum :: (Show a, Enum a) => String -> Maybe [FlagSpec a] -> (a -> Bool) -> Html
       showFlagEnum lbl specs test = do
         H.td $ do
            H.label (toHtml lbl)
            H.br
            H.select (forM_ (sortOn (getFlagName specs) (enumFrom (toEnum 0))) $ \opt -> do
               let name = getFlagName specs opt
               H.option (toHtml name)
                     ! (if not (test opt)
                           then A.style (toValue "color:red")
                           else mempty)
                     ! A.value (toValue (show (fromEnum opt)))
               ) ! (A.size (toValue "12"))


   
   H.div (H.table $ do
      H.tr $ H.td (do
         H.label (toHtml "Compiler info")
         H.br
         H.select (forM_ (compilerInfo dflags) $ \info -> do
            H.option (toHtml (fst info ++ ": " ++ snd info))
            ) ! A.size (toValue "7")
              ! A.style (toValue "width:100%")
         ) ! A.colspan (toValue "4")

      H.tr $ do
         showFlagEnum "General flags" (Just fFlags) (`gopt` dflags)
         showFlagEnum "Dump flags" Nothing (`dopt` dflags)
         showFlagEnum "Warning flags" (Just wWarningFlags) (`wopt` dflags)
         showFlagEnum "Extensions" (Just xFlags) (`xopt` dflags)

      H.tr $ do
         H.td $ do
            H.table $ do
               H.tr $ do
                  H.td $ H.label (toHtml "Verbosity level: ")
                  H.td $ H.select (forM_ [0..5] $ \(v :: Int) -> 
                     H.option (toHtml (show v))
                        ! (if v == verbosity dflags
                           then A.selected (toValue "selected")
                           else mempty)
                     ) ! A.disabled (toValue "disabled")

               H.tr $ do
                  H.td $ H.label (toHtml "Optimization level: ")
                  H.td $ H.select (forM_ [0..2] $ \(v :: Int) -> 
                     H.option (toHtml (show v))
                        ! (if v == optLevel dflags
                           then A.selected (toValue "selected")
                           else mempty)
                     ) ! A.disabled (toValue "disabled")

               H.tr $ do
                  H.td $ H.label (toHtml "Debug level: ")
                  H.td $ H.select (forM_ [0..2] $ \(v :: Int) -> 
                     H.option (toHtml (show v))
                        ! (if v == debugLevel dflags
                           then A.selected (toValue "selected")
                           else mempty)
                     ) ! A.disabled (toValue "disabled")
         H.td $ do
            H.table $ do
               H.tr $ do
                  H.td $ H.label (toHtml "Language: ")
                  -- TODO: make Language Show/Eq instances
                  let ms = enumFrom Haskell98
                  H.td $ H.select (do
                     H.option (toHtml "-")
                        ! (if isNothing (language dflags)
                           then A.selected (toValue "selected")
                           else mempty)
                     forM_ ms $ \m -> 
                        H.option (toHtml (case m of
                                 Haskell98   -> "Haskell 98"
                                 Haskell2010 -> "Haskell 2010"))
                           ! (if Just (fromEnum m) == fmap fromEnum (language dflags)
                              then A.selected (toValue "selected")
                              else mempty)
                     ) ! A.disabled (toValue "disabled")
               H.tr $ do
                  H.td $ H.label (toHtml "Safe mode: ")
                  -- TODO: make SafeHaskellMode an Enum/Bounded
                  -- let ms = [minBound..maxBound]
                  --  -- or = enumFrom Sf_None
                  let ms = [Sf_None,Sf_Unsafe,Sf_Trustworthy,Sf_Safe]
                  H.td $ H.select (forM_ ms $ \m -> 
                     H.option (toHtml (showSDoc dflags (ppr m)))
                        ! (if m == safeHaskell dflags
                           then A.selected (toValue "selected")
                           else mempty)
                     ) ! A.disabled (toValue "disabled")
      ) ! A.class_ (toValue "dynflags")


showPhase :: Int -> Compilation -> ServerPartT IO Html
showPhase compIdx comp = do
   let lookRead' x = do
         y <- look x
         case readEither y of
            Left _   -> mempty
            Right v -> return v
   idx <- lookRead' "phase"
   
   let findPhase []     mpi = return mpi
       findPhase (x:xs) mpi = case phasePhaseChildren mpi `atMay` x of
         Nothing -> mempty
         Just p  -> findPhase xs p

   phase <- findPhase idx (compilPhases comp)

   return $ do
      H.h2 $ toHtml ("Phase: " ++ phaseName phase)
      let go _ []     = return ()
          go i (p:ps) = case p of
            PhaseRawLog ls     -> showLogTable ls >> go i ps
            PhaseCoreSizeLog s -> do
               H.table (do
                  H.tr $ do
                     H.th (toHtml "After pass")
                     H.th (toHtml "Terms")
                     H.th (toHtml "Types")
                     H.th (toHtml "Coercions")
                  H.tr $ do
                     H.td $ toHtml (phaseCoreSizeName s)
                     H.td $ toHtml (show (phaseCoreSizeTerms s))
                     H.td $ toHtml (show (phaseCoreSizeTypes s))
                     H.td $ toHtml (show (phaseCoreSizeCoercions s))
                  ) ! A.class_ (toValue "phaseTable")
               go i ps
            PhaseDumpLog b -> showBlock b >> go i ps
            PhaseChild c   -> do
               H.hr
               case phaseChildren c of
                  [] -> H.p (toHtml ("Inner phase: "++show (phaseName c))
                            ) ! A.style (toValue ("text-align:center"))
                  _  -> H.p (H.a (toHtml ("Inner phase: "++show (phaseName c))
                                 ) ! A.href (toValue ("/?profile="++show compIdx ++"&page=phase&phase="++show (idx ++ [i])))
                            ) ! A.style (toValue ("text-align:center"))
               H.hr
               go (i+1) ps
      go (0 :: Int) (phaseChildren phase)


showModuleFilter :: Map Int CompilationProfile -> Map Int CompState -> [PhaseInfo] -> String -> ServerPartT IO Html
showModuleFilter profs comps phases page = do
   (cid,_) <- lookCompilation profs comps
   md <- optional $ look "module"

   return $ showBox "Filter by module:" $ do
      let mods = nub $ fmap fromJust (filter isJust (fmap phaseModule phases))
      H.table $ do
         H.tr $ H.td $ H.a (toHtml "All")
            ! A.href (toValue $ "/?profile="++show cid++"&page="++page)
            ! if not (isNothing md)
               then mempty
               else A.class_ (toValue "selectedItem")
         forM_ mods $ \md' ->
            H.tr $ H.td $ H.a (toHtml md')
               ! A.href (toValue $ "/?profile="++show cid++"&page="++page++"&module="++md')
               ! if Just md' /= md
                  then mempty
                  else A.class_ (toValue "selectedItem")

showPhases :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO (Html,Html)
showPhases profs comps = do
   (cid,comp) <- lookCompilation profs comps
   md <- optional $ look "module"

   let phases' = phasePhaseChildren (compilPhases comp)
       phases  = case md of
         Nothing -> phases'
         Just _  -> filter ((== md) . phaseModule) phases'
       totdur  = sum (phaseDuration <$> phases')
       totmem  = sum (phaseMemory <$> phases')
       go ps = do
         forM_ ps $ \s -> H.tr $ do
            let ind = concat (replicate (4*(length (phasePath s)-1)) "&nbsp;") ++ " * "
                nm  = H.preEscapedToHtml ind >> toHtml (phaseName s)
            case phaseChildren s of
               [] -> H.td nm ! A.style (toValue "text-align:left")
               _  -> H.td (H.a nm
                  ! A.href (toValue ("/?profile="++show cid ++"&page=phase&phase="++show (phasePath s)))
                  ) ! A.style (toValue "text-align:left")
            H.td $ toHtml (fromMaybe "-" (phaseModule s))
            H.td $ htmlPercent (phaseDuration s / totdur * 100)
            H.td $ htmlPercent (phaseMemory s / totmem * 100)
            H.td $ htmlFloat (phaseDuration s)
            H.td $ htmlFloat (phaseMemory s)
            go (phasePhaseChildren s)

   pageList <- showModuleFilter profs comps phases' "all-phases"

   let html = do
         H.table (do
            H.tr $ do
               H.th (toHtml "Phase")
               H.th (toHtml "Module")
               H.th (toHtml "Duration (%)")
               H.th (toHtml "Memory (%)")
               H.th (toHtml "Duration (ms)")
               H.th (toHtml "Memory (MB)")
            go phases
            H.tr (do
               H.th $ toHtml "Total"
               H.td $ toHtml "-"
               H.td $ toHtml "-"
               H.td $ toHtml "-"
               H.td $ htmlFloat totdur
               H.td $ htmlFloat totmem
               ) ! A.style (toValue "border-top: 1px gray solid")
            ) ! A.class_ (toValue "phaseTable")

   return (pageList,html)

htmlFloat :: Float -> Html
htmlFloat f = toHtml (showFFloat (Just 2) f "")

htmlPercent :: Float -> Html
htmlPercent f = toHtml (showFFloat (Just 0) f "")

showCoreSizeEvolution :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO (Html,Html)
showCoreSizeEvolution profs comps = do
   (cid,comp) <- lookCompilation profs comps

   md <- optional $ look "module"

   let phases' = phasePhaseChildren (compilPhases comp)
       phases  = case md of
         Nothing -> phases'
         Just _  -> filter ((== md) . phaseModule) phases'
       go ps = do
         forM_ ps $ \phase -> H.tr $ do
            forM_ (phaseChildren phase) $ \case
               PhaseCoreSizeLog s -> H.tr $ do 
                  H.td $ toHtml (fromMaybe "-" (phaseModule phase))
                  H.td $ H.a (toHtml (phaseName phase))
                     ! A.href (toValue ("/?profile="++show cid++"&page=phase&phase="++show (phasePath phase)))
                  H.td $ toHtml (phaseCoreSizeName s)
                  H.td $ toHtml (show (phaseCoreSizeTerms s))
                  H.td $ toHtml (show (phaseCoreSizeTypes s))
                  H.td $ toHtml (show (phaseCoreSizeCoercions s))
               _ -> return ()
            go (phasePhaseChildren phase)

   pageList <- showModuleFilter profs comps phases' "core-overview"

   let html = do
         H.p (toHtml "These are the core-to-core passes that have been applied. The number of terms, types and coercions must stay reasonable.")
         H.table (do
            H.tr $ do
               H.th (toHtml "Module")
               H.th (toHtml "Phase")
               H.th (toHtml "After pass")
               H.th (toHtml "Terms")
               H.th (toHtml "Types")
               H.th (toHtml "Coercions")
            go phases
            ) ! A.class_ (toValue "phaseTable")

   return (pageList,html)

flattenPhases :: [PhaseInfo] -> [PhaseInfo]
flattenPhases [] = []
flattenPhases pis = pis ++ flattenPhases (concatMap phasePhaseChildren pis)


showOverview :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showOverview profs comps = do
   (cid,comp) <- lookCompilation profs comps

   let ps     = phasePhaseChildren (compilPhases comp)
       totmem = sum (phaseMemory <$> ps)
       totdur = sum (phaseDuration <$> ps)

   let showStats phases = do
         let allphases = flattenPhases phases
             groups    = groupOn phaseName (sortOn phaseName allphases)
             gstat     = fmap (\group ->
               let dur  = sum (fmap phaseDuration group)
                   mem  = sum (fmap phaseMemory group)
                   durp = dur / totdur * 100
                   memp = mem / totmem * 100
               in (phaseName (head group),dur,mem,durp,memp)) groups
             gstat' = reverse $ sortOn (\(_,_,_,durp,memp) -> durp+memp) gstat
         H.table (do
            H.tr $ do
               H.th (toHtml "Phase")
               H.th (toHtml "Duration (%)")
               H.th (toHtml "Memory (%)")
               H.th (toHtml "Duration (ms)")
               H.th (toHtml "Memory (MB)")
            forM_ gstat' $ \(nam,dur,mem,durp,memp) -> H.tr $ do
               H.td $ toHtml nam
               H.td $ htmlPercent durp
               H.td $ htmlPercent memp
               H.td $ htmlFloat dur
               H.td $ htmlFloat mem
            H.tr (do
               H.th $ toHtml "Total"
               H.td $ toHtml "-"
               H.td $ toHtml "-"
               H.td $ htmlFloat totdur
               H.td $ htmlFloat totmem
               ) ! A.style (toValue "border-top: 1px gray solid")
            ) ! A.class_ (toValue "phaseTable")

   let showPerModule phases = do
         let gstat = fmap (\phase ->
               let dur  = phaseDuration phase
                   mem  = phaseMemory phase
                   durp = dur / totdur * 100
                   memp = mem / totmem * 100
               in (fromJust (phaseModule phase),dur,mem,durp,memp)) phases
             gstat' = reverse $ sortOn (\(_,_,_,durp,memp) -> durp+memp) gstat
         H.table (do
            H.tr $ do
               H.th (toHtml "Module")
               H.th (toHtml "Duration (%)")
               H.th (toHtml "Memory (%)")
               H.th (toHtml "Duration (ms)")
               H.th (toHtml "Memory (MB)")
            forM_ gstat' $ \(md,dur,mem,durp,memp) -> H.tr $ do
               H.td $ H.a (toHtml md)
                  ! A.href (toValue $ "/?profile="++show cid++"&page=all-phases&module="++md)
               H.td $ htmlPercent durp
               H.td $ htmlPercent memp
               H.td $ htmlFloat dur
               H.td $ htmlFloat mem
            ) ! A.class_ (toValue "phaseTable")
         
   return $ do
      H.h2 (toHtml "Overall")
      showStats ps
      H.h2 (toHtml "Per module")
      showPerModule (filter (isJust . phaseModule) ps)

makePhaseInfos :: [Log] -> PhaseInfo
makePhaseInfos ls = case runParser parseChildren "logs" ls of
      Right cs -> emptyPhaseInfo { phaseChildren = cs }
      Left  e  -> error (show e)
   where
      parseChildren :: LogParser [PhaseChildType]
      parseChildren = do
         children <- concat <$> manyTill parseChild
                        (try (lookAhead (void (toLogParser parseEnd))) <|> eof)
         let
         -- fusion consecutive raw logs
            f (PhaseRawLog xs) (rs,cs) = (xs++rs,cs)
            f c                ([],cs) = ([],c:cs)
            f c                (rs,cs) = ([], c:PhaseRawLog rs:cs)
            children' = case foldr f ([],[]) children of
               ([],cs) -> cs
               (rs,cs) -> PhaseRawLog rs:cs
         -- prepend child id
            prependPath x = \case
               PhaseChild p -> PhaseChild $ p
                  { phasePath     = (x:phasePath p)
                  , phaseChildren = fmap (prependPath x) (phaseChildren p)
                  }
               c            -> c
            prepgo _ []                    = []
            prepgo n (x@(PhaseChild _):xs) = prependPath n x : prepgo (n+1) xs
            prepgo n (x:xs)                = x : prepgo n xs
            children'' = prepgo 0 children'
         return $! children''
      
      parsePhase :: LogParser PhaseInfo
      parsePhase = do
         (name,md) <- toLogParser parseBegin
         children  <- parseChildren
         (dur,mem) <- toLogParser parseEnd
         return $! PhaseInfo name md dur mem children []

      wrap x = [x]

      parseChild :: LogParser [PhaseChildType]
      parseChild = choice
         [ (wrap . PhaseChild      )   <$> try parsePhase
         , (wrap . PhaseCoreSizeLog)   <$> try (toLogParser parseCoreSizeLog)
         , fmap PhaseDumpLog           <$> parseDumpLog
         , (wrap . PhaseRawLog . wrap) <$> satisfyLog (const True)
         ]

      parsePhaseName :: Parser (String,Maybe String)
      parsePhaseName = 
         (try $ do
            phase <- manyTill anyChar (string " [")
            md    <- manyTill anyChar (char ']')
            void (char ':')
            let md' = if null md then Nothing else Just md
            return (phase, md')
         ) <|> (do
            phase <- manyTill anyChar (char ':')
            return (phase, Nothing)
         )

      parseBegin :: Parser (String,Maybe String)
      parseBegin = do
         void (string "*** ")
         (name,md) <- parsePhaseName
         return (name,md)


      toLogParser :: Show a => Parser a -> LogParser a
      toLogParser p = token f Nothing
         where
            f x = case parseMaybe p (showLogMessage x) of
                     Nothing -> Left (Set.singleton (Tokens (x:|[])), Set.empty, Set.empty) 
                     Just y  -> Right y
            

      parseEnd :: Parser (Float,Float)
      parseEnd = do
         void (string "!!! ")
         void $ parsePhaseName
         void (string " finished in ")
         dur   <- read <$> manyTill anyChar (char ' ')
         void (string "milliseconds, allocated ")
         mem   <- read <$> manyTill anyChar (char ' ')
         void (string "megabytes")
         return (dur, mem)

      parseCoreSizeLog :: Parser PhaseCoreSize
      parseCoreSizeLog = do
         void (string "Core size after ")
         phase <- manyTill anyChar (try (string " = {terms: "))
         -- be careful, ',' is used as a field separator and as a thousands
         -- separator.
         terms <- (read . replace "," "") <$> manyTill anyChar (string ", types: ")
         typs  <- (read . replace "," "") <$> manyTill anyChar (string ", coercions: ")
         coes  <- (read . replace "," "") <$> manyTill anyChar (char '}')
         return $ PhaseCoreSize phase terms typs coes

      
      -- satisfyLog :: (Log -> Bool) -> m Log
      satisfyLog f = token testChar Nothing
        where
          testChar x =
            if f x
              then Right x
              else Left (Set.singleton (Tokens (x:|[]))
                        , Set.empty :: Set.Set (ErrorItem Log)
                        , Set.empty :: Set.Set Dec)


      parseDumpLog :: LogParser [Block]
      parseDumpLog = do
         let isSevDump SevDump = True
             isSevDump _       = False
         l <- satisfyLog (isSevDump . logSeverity)
         let contents = showLogMessage l
         return $ case runParser blocks "" contents of
            Right x -> x
            Left e  -> [Block "Bogus dump" Nothing
                        (contents ++ "\n\n==== Parse error ====\n" ++ show e)]
      blockMark = do
         void eol
         void $ count 20 (char '=')
         void $ char ' '
         let endmark = (char ' ' >> count 20 (char '=') >> eol)
         bname <- manyTill anyChar (try endmark)
         return bname
      
      blockHead = do
         bname <- blockMark
         dateStr <- lookAhead (manyTill anyChar (eol <|> (eof >> return "")))
         -- date isn't always present (e.g., typechecker dumps)
         date <- if "UTC" `isSuffixOf` dateStr
            then do
               void (manyTill anyChar eol)
               void eol
               return (Just (read dateStr))
            else return Nothing
         return (bname,date)
      
      block = do
         (name,date) <- blockHead
         bcontents <- manyTill anyChar (try (lookAhead (void blockMark)) <|> eof)
         return (Block name date bcontents)
      
      blocks :: Parser [Block]
      blocks = many block <* eof



showBlock :: Block -> Html
showBlock block = do
   H.h4 (toHtml (blockName block))
   case blockDate block of
         Nothing -> return ()
         Just x  -> H.div (toHtml (show x)) ! A.class_ (toValue "date")
   H.div $ toHtml
      $ formatHtmlBlock defaultFormatOpts
      $ highlightAs (selectFormat (blockName block)) (blockContents block)

-- | Select highlighting format
selectFormat :: String -> String
selectFormat name
   | "Asm -"               `isPrefixOf` name   = "nasm"
   | "Haskell -"           `isPrefixOf` name   = "haskell"
   | "STG -"               `isPrefixOf` name   = "haskell"
   | "Core -"              `isPrefixOf` name   = "haskell"
   | "C -"                 `isPrefixOf` name   = "c"
   | "Cmm -"               `isPrefixOf` name   = "c"
   | "Statistics -"        `isPrefixOf` name   = "c"

   | "Asm code"             `isInfixOf` name   = "nasm"
   | "Synthetic instructions expanded" == name = "nasm"
   | "Registers allocated"             == name = "nasm"
   | "Liveness annotations added"      == name = "nasm"
   | "Native code"                     == name = "nasm"
   | "Sink assignments"                == name = "c"
   | "Layout Stack"                    == name = "c"
   | "Post switch plan"                == name = "c"
   | "Post common block elimination"   == name = "c"
   | "Post control-flow optimisations" == name = "c"
   | "after setInfoTableStackMap"      == name = "c"
   | "Cmm"                  `isInfixOf` name   = "c"
   | "Foreign export"       `isInfixOf` name   = "c"
   | "CAFEnv"                          == name = "haskell"
   | "Common sub-expression"           == name = "haskell"
   | "Occurrence analysis"             == name = "haskell"
   | "worker Wrapper binds"            == name = "haskell"
   | "Demand analysis"                 == name = "haskell"
   | "Called arity analysis"           == name = "haskell"
   | "Specialise"                      == name = "haskell"
   | "Desugar"              `isInfixOf` name   = "haskell"
   | "STG"                  `isInfixOf` name   = "haskell"
   | "Simplifier"          `isPrefixOf` name   = "haskell"
   | "Parser"                          == name = "haskell"
   | "Derived instances"               == name = "haskell"
   | otherwise                                 = ""
