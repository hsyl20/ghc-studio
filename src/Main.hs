{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import CmdLine (Options(..), getOptions)

import Control.Monad
import Text.Printf
import Network.Socket (withSocketsDo)
import Happstack.Server
import Data.List (isPrefixOf, isSuffixOf, sortOn, isInfixOf, intersperse)
import Data.Maybe (fromJust, isJust, fromMaybe, mapMaybe, isNothing)
import Data.FileEmbed
import System.IO.Temp
import System.FilePath
import Data.IORef
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Map (Map)

import Text.Highlighting.Kate as Kate
import Text.Megaparsec.String (Parser)
import Text.Megaparsec
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


data Compilation = Compilation
   { compilFlags   :: DynFlags
   , compilSources :: [File]
   , compilLogs    :: [Log]
   , compilPhases  :: PhaseInfo
   }

data Log = Log
   { logDynFlags :: DynFlags
   , logReason   :: WarnReason
   , logSeverity :: Severity
   , logLocation :: Maybe Location
   , logStyle    :: PprStyle
   , logMessage  :: SDoc
   }

data Location = Location
   { locFile      :: String
   , locStartLine :: Int
   , locEndLine   :: Int
   , locStartCol  :: Int
   , locEndCol    :: Int
   }

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

      , dir "quit" $ nullDir >> do
         liftIO $ putMVar quit ()
         tempRedirect "/" (toResponse "")

      , nullDir >> do
         ps <- liftIO $ atomically $ readTVar profs
         ok $ toResponse $ appTemplate "Welcome" $ showWelcome infiles ps

      , dir "manager" $ do
         ps   <- liftIO $ readTVarIO profs
         cs   <- liftIO $ readTVarIO comps
         html <- showManager infiles ps cs
         ok $ toResponse $ appTemplate "" html

      , dir "file" $ uriRest $ \p' -> do
         let p = if "/" `isPrefixOf` p' then tail p' else p'
         case filter ((==p) . fileName) infiles of
            []    -> mempty
            (x:_) -> ok $ toResponse $ appTemplate ("File: " ++ p) $ showInputFile x

      , dir "compilation" $ path $ \i -> do
         ps <- liftIO $ readTVarIO profs
         case Map.lookup i ps of
            Nothing   -> mempty
            Just prof -> do
               needCompile <- liftIO $ atomically $ do
                  cs <- readTVar comps
                  if Map.notMember i cs
                     then do
                        modifyTVar comps (Map.insert i Compiling)
                        return True
                     else return False
               
               when needCompile $ liftIO $ void $ forkIO $ do
                  comp <- compileFiles infiles prof
                  atomically $ modifyTVar comps (Map.insert i (Compiled comp))

               cs' <- liftIO $ readTVarIO comps
               let title = profileName prof
               let
                  findPhase :: Compilation -> [Int] -> PhaseInfo -> ServerPartT IO Response
                  findPhase c idx mpi = msum
                     [ path $ \n -> case phasePhaseChildren mpi `atMay` n of
                           Nothing -> mempty
                           Just p  -> findPhase c (n:idx) p
                     , nullDir >> (ok $ toResponse $ appTemplate title
                                      $ showPhase i c (reverse idx) mpi)
                     ]
                     
               case Map.lookup i cs' of
                  Nothing             -> mempty
                  Just (Compiling {}) -> do
                     ok $ toResponse $ showCompiling title i
                  Just (Compiled c)   -> msum
                     [ nullDir >> (ok $ toResponse $ appTemplate title $
                        showCompilation i c)
                     , dir "phase" $ findPhase c [] (compilPhases c)
                     , dir "logs" $ do
                        html <- showLogs infiles c
                        ok $ toResponse $ appTemplate title $ html
                     ]
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


compileFiles :: [File] -> CompilationProfile -> IO Compilation
compileFiles files prof = do
   logs <- newIORef []

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
                  modifyIORef logs (Log dfl reason sev (convertLocation srcspan) style msg :)
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

   let phaseInfos = makePhaseInfos logs'

   return (phaseInfos `seq` Compilation dflgs files logs' phaseInfos)



css :: Response
css = toResponseBS (C.pack "text/css") (L.fromStrict $(embedFile "src/style.css"))

js :: Response
js = toResponseBS (C.pack "text/javascript") (L.fromStrict $(embedFile "src/script.js"))

-- | Template of all pages
appTemplate :: String -> Html -> Html
appTemplate title bdy = docTypeHtml $ do
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
   H.body $ do
      H.div (toHtml $ "GHC Studio " ++ if null title then "" else " / " ++ title)
         ! A.class_ (toValue "headtitle")
      H.div (do
         H.a (toHtml ("Home")) ! A.href (toValue "/")
         toHtml "  -  "
         H.a (toHtml ("Quit")) ! A.href (toValue "/quit")
         ) ! A.class_ (toValue "panel")
      bdy

-- | Welcoming screen
showWelcome :: [File] -> Map Int CompilationProfile -> Html
showWelcome files profs = do
   H.p (toHtml "This is a GHC Web frontend. It will help you debug your program and/or GHC.")
   H.p (toHtml "The following files are considered:")
   H.ul $ forM_ files $ \file -> do
      H.li $ H.a (toHtml (fileName file))
         ! A.href (toValue ("/file/"++fileName file))
   H.p (toHtml "Now you can compile your files with the profile you want:")
   H.table $ forM_ (Map.toList profs) $ \(i,prof) -> do
      H.tr $ do
         H.td $ H.a (toHtml (profileName prof)) ! A.href (toValue ("/compilation/"++show i))
         H.td $ toHtml (profileDesc prof)

showManager :: [File] -> Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showManager files profs comps = do
   profList <- showProfileList profs comps
   compList <- showCompilePageList profs comps
   page     <- showPage files profs comps

   return $ do
      H.table $ H.tr $ do
         H.td (profList >> compList) ! A.style (toValue "vertical-align:top")
         H.td page ! A.style (toValue "vertical-align:top")

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

getCompilation :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO (Maybe (Int,CompState))
getCompilation profs comps = do
   getSelectedProfile profs >>= \case
      Nothing    -> return Nothing
      Just (i,_) -> case Map.lookup i comps of
         Nothing -> return Nothing
         Just c  -> return (Just (i,c))

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
   let html =  showBox "Runs" $ H.table $ do
         forM_ (Map.toList ps) $ \(i,prof) -> H.tr $ do
            H.td $ H.a (toHtml (profileName prof))
               ! A.href (toValue ("/manager?profile="++show i))
               ! if Just i /= selProf
                  then mempty
                  else A.class_ (toValue "selectedItem")
         H.td $ H.a (toHtml "* New run *")
               ! A.href (toValue ("/manager?profile=new"))
               ! if not newProf
                  then mempty
                  else A.class_ (toValue "selectedItem")
   return html

uriSelectCompilePage :: Map Int CompilationProfile -> Map Int CompState -> String -> ServerPartT IO H.AttributeValue
uriSelectCompilePage profs comps page = do
   selComp <- getCompilation profs comps
   case selComp of
      Just (p,_) -> return $ toValue $ "/manager?profile="++show p++"&page="++page
      Nothing    -> mempty

showCompilePageList :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showCompilePageList profs comps = do
   getCompilation profs comps >>= \case
      Nothing -> return (return ())
      Just c  -> do -- TODO: only display pages valid for the given compil
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
         uriFullLog      <- makeItem "Full log" "full-log"
      
         return $ showBox "Run infos" $ H.table $ do
            H.tr $ H.td $ uriConf
            H.tr $ H.td $ uriOverview
            H.tr $ H.td $ uriCoreOverview
            H.tr $ H.td $ uriAllPhases
            H.tr $ H.td $ uriIR
            H.tr $ H.td $ uriFullLog

showPage :: [File] -> Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showPage files profs comps = do
   selPage <- optional (look "page")
   case selPage of
      Nothing -> getSelectedProfile profs >>= \case
         Just (_,p) -> showProfile p
         Nothing    -> isNewProfile >>= \case
            True  -> showProfileListPage profs comps
            False -> showDefault files profs comps
      Just p  -> getCompilation profs comps >>= \case
         Nothing    -> mempty
         Just (ci,cstate) -> case cstate of
            Compiling  -> return (toHtml "Compiling")
            Compiled c -> case p of
               "config"        -> return $ showDynFlags (compilFlags c)
               "overview"      -> return $ showPhasesSummary c
               "core-overview" -> return (showCoreSizeEvolution ci c)
               "all-phases"    -> return (showPhases ci c)
               "full-log"      -> showLogs files c
               _               -> return (toHtml "TODO")

showDefault :: [File] -> Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showDefault _ _ _ = return $ do
   H.p $ toHtml "GHC studio helps you debug your program and/or GHC."
   H.p $ toHtml "Start a compilation by choosing \"New run\" on the left"

showProfileListPage :: Map Int CompilationProfile -> Map Int CompState -> ServerPartT IO Html
showProfileListPage profs _comps = return $ do
   H.table $ forM_ (Map.toList profs) $ \(i,prof) -> do
      H.tr $ do
         H.td $ H.a (toHtml (profileName prof)) 
            ! A.href (toValue ("/compilation/"++show i))
         H.td $ toHtml (profileDesc prof)

showProfile :: CompilationProfile -> ServerPartT IO Html
showProfile prof = return $ toHtml (profileName prof)

-- | Template of all pages
showCompiling :: String -> Int -> Html
showCompiling title compIdx = docTypeHtml $ do
   H.head $ do
      H.title (toHtml "GHC Web")
      H.meta ! A.httpEquiv (toValue "Content-Type")
             ! A.content   (toValue "text/html;charset=utf-8")
      H.meta ! A.httpEquiv (toValue "refresh")
             ! A.content   (toValue "2")
      H.link ! A.rel       (toValue "stylesheet") 
             ! A.type_     (toValue "text/css")
             ! A.href      (toValue "/css/style.css")
      H.style ! A.type_ (toValue "text/css") $ toHtml $ styleToCss tango
   H.body $ do
      H.div (toHtml $ "GHC Web " ++ " / " ++ title)
         ! A.class_ (toValue "headtitle")
      H.div (do
         H.a (toHtml ("Home")) ! A.href (toValue "/")
         toHtml "  -  "
         H.a (toHtml ("Quit")) ! A.href (toValue "/quit")
         ) ! A.class_ (toValue "panel")
   toHtml "Compiling..."
   H.br
   H.a (toHtml "Refresh")
      ! A.href (toValue ("/compilation/"++show compIdx))

showInputFile :: File -> Html
showInputFile file = do
   H.h2 (toHtml (fileName file))
   H.div $ toHtml
      $ formatHtmlBlock defaultFormatOpts
      $ highlightAs "haskell" (fileContents file)

showCompilation :: Int -> Compilation -> Html
showCompilation i comp = do
   H.h1 (toHtml "GHC configuration used for this build")
   showDynFlags (compilFlags comp)
   H.h1 (toHtml "Analysis")

   H.h2 (toHtml "Phases: summary")
   showPhasesSummary comp

   H.h2 (toHtml "Core Phases: summary")
   showCoreSizeEvolution i comp

   H.h2 (toHtml "Phases: details")
   showPhases i comp
   H.br

   H.div
      (H.a (toHtml "View full log")
            ! A.href (toValue ("/compilation/"++show i++"/logs"))
      ) ! A.style (toValue "margin:auto; text-align:center")

   H.h2 (toHtml "Sources with inline logs:")
   H.table (forM_ (compilSources comp) $ \f -> H.tr $ do
      H.td $ H.a (toHtml (toHtml (fileName f)))
         ! A.href (toValue ("/compilation/"++show i++"/logs?file="++fileName f))
      ) ! A.class_ (toValue "phaseTable")


showLogs :: [File] -> Compilation -> ServerPartT IO Html
showLogs files comp = do
   fileFilter <- optional $ look "file"
   prof <- look "profile"

   let filterHtml = do
         toHtml "Restrict to file:"
         H.ul $ do
            H.li $ H.a (toHtml "All")
               ! A.href (toValue ("/manager?profile="++prof++"&page=full-log"))
               ! if fileFilter /= Nothing
                  then mempty
                  else A.class_ (toValue "selectedItem")
            forM_ files $ \file -> do
               H.li $ H.a (toHtml (fileName file))
                  ! A.href (toValue ("/manager?profile="++prof++"&page=full-log&file="++fileName file))
                  ! if fileFilter /= Just (fileName file)
                     then mempty
                     else A.class_ (toValue "selectedItem")

   return $ do
      filterHtml
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
            H.td $ toHtml $ case logSeverity clog of
               SevOutput      -> "Output"
               SevFatal       -> "Fatal"
               SevInteractive -> "Interactive"
               SevDump        -> "Dump"
               SevInfo        -> "Info"
               SevWarning     -> "Warning"
               SevError       -> "Error"
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


data PhaseBegin = PhaseBegin
   { phaseBeginName   :: String
   , phaseBeginModule :: Maybe String
   } deriving (Show)

data PhaseStat = PhaseStat
   { phaseStatDuration :: Float
   , phaseStatMemory   :: Float
   } deriving (Show)

data PhaseCoreSize = PhaseCoreSize
   { phaseCoreSizeName      :: String
   , phaseCoreSizeTerms     :: Word
   , phaseCoreSizeTypes     :: Word
   , phaseCoreSizeCoercions :: Word
   } deriving (Show)

data PhaseChildType
   = PhaseRawLog [Log]
   | PhaseCoreSizeLog PhaseCoreSize
   | PhaseDumpLog Block
   | PhaseChild PhaseInfo

data PhaseInfo = PhaseInfo
   { phaseName        :: String
   , phaseModule      :: Maybe String
   , phaseDuration    :: Float
   , phaseMemory      :: Float
   , phaseChildren    :: [PhaseChildType]
   }

emptyPhaseInfo :: PhaseInfo
emptyPhaseInfo = PhaseInfo
   { phaseName     = ""
   , phaseModule   = Nothing
   , phaseDuration = 0
   , phaseMemory   = 0
   , phaseChildren = []
   }
   

showPhase :: Int -> Compilation -> [Int] -> PhaseInfo -> Html
showPhase compIdx _ idx phase = do
   H.h2 $ toHtml ("Phase: " ++ phaseName phase)
   let pth = concat (intersperse "/" (fmap show idx)) ++ "/"
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
                              ) ! A.href (toValue ("/compilation/"++show compIdx ++"/phase/"++pth++show i))
                         ) ! A.style (toValue ("text-align:center"))
            H.hr
            go (i+1) ps
   go (0 :: Int) (phaseChildren phase)

showPhases :: Int -> Compilation -> Html
showPhases compIdx comp = do
   let phases = phasePhaseChildren (compilPhases comp)
       totdur = sum (phaseDuration <$> phases)
       totmem = sum (phaseMemory <$> phases)
       go ps parents = do
         let pth = concat (intersperse "/" (fmap show (reverse parents))) ++ if null parents then "" else "/"
             ind = concat (replicate (4*length parents) "&nbsp;") ++ " * "
             nm p = H.preEscapedToHtml ind >> toHtml (phaseName p)
         forM_ (ps `zip` [0..]) $ \(s,(idx :: Int)) -> H.tr $ do
            case phaseChildren s of
               [] -> H.td (nm s) ! A.style (toValue "text-align:left")
               _  -> H.td (H.a (nm s)
                  ! A.href (toValue ("/compilation/"++show compIdx ++"/phase/"++pth++show idx))
                  ) ! A.style (toValue "text-align:left")
            H.td $ toHtml (fromMaybe "-" (phaseModule s))
            H.td $ htmlPercent (phaseDuration s / totdur * 100)
            H.td $ htmlPercent (phaseMemory s / totmem * 100)
            H.td $ htmlFloat (phaseDuration s)
            H.td $ htmlFloat (phaseMemory s)
            go (phasePhaseChildren s) (idx:parents)

   H.table (do
      H.tr $ do
         H.th (toHtml "Phase")
         H.th (toHtml "Module")
         H.th (toHtml "Duration (%)")
         H.th (toHtml "Memory (%)")
         H.th (toHtml "Duration (ms)")
         H.th (toHtml "Memory (MB)")
      go phases []
      H.tr (do
         H.th $ toHtml "Total"
         H.td $ toHtml "-"
         H.td $ toHtml "-"
         H.td $ toHtml "-"
         H.td $ htmlFloat totdur
         H.td $ htmlFloat totmem
         ) ! A.style (toValue "border-top: 1px gray solid")
      ) ! A.class_ (toValue "phaseTable")

htmlFloat :: Float -> Html
htmlFloat f = toHtml (showFFloat (Just 2) f "")

htmlPercent :: Float -> Html
htmlPercent f = toHtml (showFFloat (Just 0) f "")

showCoreSizeEvolution :: Int -> Compilation -> Html
showCoreSizeEvolution compIdx comp = do
   let phases = phasePhaseChildren (compilPhases comp)
       go ps parents = do
         let pth = concat (intersperse "/" (fmap show (reverse parents))) ++ if null parents then "" else "/"
         forM_ (ps `zip` [0..]) $ \(phase,(pid :: Int)) -> H.tr $ do
            forM_ (phaseChildren phase) $ \case
               PhaseCoreSizeLog s -> H.tr $ do 
                  H.td $ toHtml (fromMaybe "-" (phaseModule phase))
                  H.td $ H.a (toHtml (phaseName phase))
                     ! A.href (toValue ("/compilation/"++show compIdx++"/phase/"++pth++show pid))
                  H.td $ toHtml (phaseCoreSizeName s)
                  H.td $ toHtml (show (phaseCoreSizeTerms s))
                  H.td $ toHtml (show (phaseCoreSizeTypes s))
                  H.td $ toHtml (show (phaseCoreSizeCoercions s))
               _ -> return ()
            go (phasePhaseChildren phase) (pid:parents)

   H.p (toHtml "These are the core-to-core passes that have been applied. The number of terms, types and coercions must stay reasonable.")
   H.table (do
      H.tr $ do
         H.th (toHtml "Module")
         H.th (toHtml "Phase")
         H.th (toHtml "After pass")
         H.th (toHtml "Terms")
         H.th (toHtml "Types")
         H.th (toHtml "Coercions")
      go phases []
      ) ! A.class_ (toValue "phaseTable")

flattenPhases :: [PhaseInfo] -> [PhaseInfo]
flattenPhases [] = []
flattenPhases pis = pis ++ flattenPhases (concatMap phasePhaseChildren pis)


showPhasesSummary :: Compilation -> Html
showPhasesSummary comp = do
   let allphases = flattenPhases phases
       phases    = phasePhaseChildren (compilPhases comp)
       groups    = groupOn phaseName (sortOn phaseName allphases)
       totmem    = sum (phaseMemory <$> phases)
       totdur    = sum (phaseDuration <$> phases)
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

makePhaseInfos :: [Log] -> PhaseInfo
makePhaseInfos = head . go [emptyPhaseInfo { phaseName = "" }]
   where
      go :: [PhaseInfo] -> [Log] -> [PhaseInfo]
      go []  _         = error "Unexpected missing phase infos"
      go [p] []        = [reverseChildren p]
      go _   []        = error "Unexpected end of logs"
      go (p:ps) (l:ls) =
         let msg = logMessage' l in
         case parseMaybe parsePhaseBegin msg of
            -- add the phase to the list
            Just b  -> let np = emptyPhaseInfo
                                 { phaseName     = phaseBeginName b
                                 , phaseModule   = phaseBeginModule b
                                 }
                       in go (np:p:ps) ls
            Nothing -> do
               case parseMaybe parsePhaseCoreSize msg of
                  Just s  -> go (appendChild (PhaseCoreSizeLog s) p:ps) ls
                  Nothing -> case parseMaybe parsePhaseStat msg of
                     -- phase end: add it to the parent children
                     Just s -> let np = reverseChildren $ p
                                       { phaseDuration = phaseStatDuration s
                                       , phaseMemory   = phaseStatMemory   s
                                       }
                                in case ps of
                                    (parent:ps') -> go (addChild np parent:ps') ls
                                    _            -> error "Unexpected phase infos"
                     Nothing -> case logSeverity l of
                        SevDump -> let blks = PhaseDumpLog <$> parseBlock "" msg
                                   in go (foldl (flip appendChild) p blks:ps) ls
                        _       -> case phaseChildren p of
                           -- concat consecutive raw logs
                           (PhaseRawLog rl:rs) ->
                              let np = p
                                    { phaseChildren = PhaseRawLog (rl++[l]) : rs
                                    }
                               in go (np:ps) ls

                           _                  -> go (appendChild (PhaseRawLog [l]) p:ps) ls
                        

      reverseChildren :: PhaseInfo -> PhaseInfo
      reverseChildren phi = phi { phaseChildren = reverse (phaseChildren phi)}

      appendChild :: PhaseChildType -> PhaseInfo -> PhaseInfo
      appendChild l phi = phi { phaseChildren = l:phaseChildren phi }

      addChild :: PhaseInfo -> PhaseInfo -> PhaseInfo
      addChild c = appendChild (PhaseChild c)

      logMessage' l = showSDoc (logDynFlags l) (logMessage l)

      parsePhaseName :: Parser (String,Maybe String)
      parsePhaseName = 
         (try $ do
            phase <- manyTill anyChar (string " [")
            md    <- manyTill anyChar (char ']')
            void (char ':')
            return (phase, Just md)
         ) <|> (do
            phase <- manyTill anyChar (char ':')
            return (phase, Nothing)
         )

      parsePhaseBegin :: Parser PhaseBegin
      parsePhaseBegin = do
         void (string "*** ")
         (phase,md) <- parsePhaseName
         return $ PhaseBegin phase md

      parsePhaseStat :: Parser PhaseStat
      parsePhaseStat = do
         void (string "!!! ")
         void $ parsePhaseName
         void (string " finished in ")
         dur   <- read <$> manyTill anyChar (char ' ')
         void (string "milliseconds, allocated ")
         mem   <- read <$> manyTill anyChar (char ' ')
         void (string "megabytes")
         return $ PhaseStat dur mem

      parsePhaseCoreSize :: Parser PhaseCoreSize
      parsePhaseCoreSize = do
         void (string "Core size after ")
         phase <- manyTill anyChar (try (string " = {terms: "))
         -- be careful, ',' is used as a field separator and as a thousands
         -- separator.
         terms <- (read . replace "," "") <$> manyTill anyChar (string ", types: ")
         typs  <- (read . replace "," "") <$> manyTill anyChar (string ", coercions: ")
         coes  <- (read . replace "," "") <$> manyTill anyChar (char '}')
         return $ PhaseCoreSize phase terms typs coes

showBlock :: Block -> Html
showBlock block = do
   H.h4 (toHtml (blockName block))
   let dateStr = case blockDate block of
         Nothing -> "No date"
         Just x  -> show x
   H.div (toHtml dateStr) ! A.class_ (toValue "date")
   H.div $ toHtml
      $ formatHtmlBlock defaultFormatOpts
      $ highlightAs (selectFormat (blockFile block) (blockName block)) (blockContents block)

-- | Select highlighting format
selectFormat :: FilePath -> String -> String
selectFormat pth name
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
   | ".dump-asm"            `isPrefixOf` ext   = "nasm"
   | ".dump-cmm"            `isPrefixOf` ext   = "c"
   | ".dump-opt-cmm"        `isPrefixOf` ext   = "c"
   | ".dump-ds"             `isPrefixOf` ext   = "haskell"
   | ".dump-occur"          `isPrefixOf` ext   = "haskell"
   | ".dump-stranal"        `isPrefixOf` ext   = "haskell"
   | ".dump-spec"           `isPrefixOf` ext   = "haskell"
   | ".dump-cse"            `isPrefixOf` ext   = "haskell"
   | ".dump-call-arity"     `isPrefixOf` ext   = "haskell"
   | ".dump-worker-wrapper" `isPrefixOf` ext   = "haskell"
   | ".dump-parsed"         `isPrefixOf` ext   = "haskell"
   | ".dump-prep"           `isPrefixOf` ext   = "haskell"
   | ".dump-stg"            `isPrefixOf` ext   = "haskell"
   | ".dump-simpl"          `isPrefixOf` ext   = "haskell"
   | ".dump-foreign"        `isPrefixOf` ext   = if "header file" `isSuffixOf` name
                                                     then "c"
                                                     else "haskell"
   | otherwise                                 = ""
   where
      ext = takeExtension pth
   
   


data Block = Block
   { blockFile     :: String
   , blockName     :: String
   , blockDate     :: Maybe UTCTime
   , blockContents :: String
   }

parseBlock :: String -> String -> [Block]
parseBlock name contents = case runParser blocks name contents of
      Right x -> x
      Left e  -> [Block name "Whole file"
                  Nothing
                  (contents ++ "\n\n==== Parse error ====\n" ++ show e)]
   where
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
         (bname,date) <- blockHead
         bcontents <- manyTill anyChar (try (lookAhead (void blockMark)) <|> eof)
         return (Block name bname date bcontents)

      blocks :: Parser [Block]
      blocks = many block


