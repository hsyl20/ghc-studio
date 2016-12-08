{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import CmdLine (Options(..), getOptions)

import Control.Monad
import Text.Printf
import Network.Socket (withSocketsDo)
import Happstack.Server
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import Data.FileEmbed
import System.IO.Temp
import System.FilePath
import Data.IORef
import Data.Foldable
import Control.Monad.IO.Class
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import Text.Highlighting.Kate
import Text.Megaparsec.String (Parser)
import Text.Megaparsec
import Text.Blaze.Html5 ((!), toHtml, docTypeHtml, Html, toValue)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import GHC
import GHC.Paths ( libdir )
import DynFlags
import System.Posix.IO
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import System.Directory
import Data.Time.Clock
import Safe


data Compilation = Compilation
   { compilFlags   :: DynFlags
   , compilSources :: [File]
   , compilStdErr  :: String
   , compilStdOut  :: String
   , compilDumps   :: [File]
   }

data CompilationProfile = CompilationProfile
   { profileName  :: String
   , profileDesc  :: String
   , profileFlags :: DynFlags -> DynFlags
   }

defaultProfiles :: [CompilationProfile]
defaultProfiles =
   [ CompilationProfile
      { profileName  = "Maximal verbosity"
      , profileDesc  = "Use maximal verbosity (-v5): the intermediate representation after each compilation phase is dumped"
      , profileFlags = \dflags -> dflags
         { verbosity = 5
         }
      }
   , CompilationProfile
      { profileName  = "Maximal verbosity and optimization"
      , profileDesc  = "Use maximal verbosity (-v5) and maximal optimization (-O2)"
      , profileFlags = \dflags -> updOptLevel 2 $ dflags
         { verbosity = 5
         }
      }
   , CompilationProfile
      { profileName  = "Debug TypeChecker"
      , profileDesc  = "Enable type-checker tracing"
      , profileFlags = \dflags -> dflags
         { verbosity = 1
         } `dopt_set` Opt_D_dump_tc
           `dopt_set` Opt_D_dump_tc_trace
      }
   ]

main :: IO ()
main = withSocketsDo $ do

   opts <- getOptions

   let infiles = 
         [ File "Main.hs"
            "import A\n\
            \main :: IO ()\n\
            \main = putStrLn astring"
         , File "A.hs"
            "module A where\n\
            \astring :: String\n\
            \astring = \"Hey!\""
         ]

   comps <- newTVarIO Map.empty
   profs <- newTVarIO defaultProfiles

   quit <- newEmptyMVar

   let conf = nullConf {port = optport opts}
   putStrLn (printf "Starting Web server at localhost:%d" (port conf))
   httpTID <- forkIO $ simpleHTTP conf $ msum
      [ dir "css" $ dir "style.css" $ ok css

      , dir "quit" $ nullDir >> do
         liftIO $ putMVar quit ()
         tempRedirect "/" (toResponse "")

      , nullDir >> do
         ps <- liftIO $ atomically $ readTVar profs
         ok $ toResponse $ appTemplate "Welcome" $ showWelcome infiles ps

      , dir "compilation" $ path $ \i -> do
         ps <- liftIO $ readTVarIO profs
         case ps `atMay` i of
            Nothing   -> mempty
            Just prof -> do
               cs <- liftIO $ readTVarIO comps
               when (Map.notMember i cs) $ do
                  comp <- liftIO $ compileFiles infiles prof
                  liftIO $ atomically $ modifyTVar comps (Map.insert i comp)

               cs' <- liftIO $ readTVarIO comps
               let title = profileName prof
               fileFilter <- optional $ look "file"
               case Map.lookup i cs' of
                  Nothing -> mempty
                  Just c  -> msum
                     [ nullDir >> (ok $ toResponse $ appTemplate title $
                        showCompilation i c)
                     , dir "all"    $ ok $ toResponse $ appTemplate title $
                        showAll fileFilter c
                     , dir "sorted" $ ok $ toResponse $ appTemplate title $
                        showSortedBlocks fileFilter c
                     ]
      ]

   takeMVar quit
   killThread httpTID
   putStrLn "Quit."


data File = File
   { fileName     :: String
   , fileContents :: String
   } deriving (Show)


compileFiles :: [File] -> CompilationProfile -> IO Compilation
compileFiles files prof = do
   putStrLn "Compiling..."

   -- GHC writes to stdout/stderr. We capture this. 
   nstdout <- dup stdOutput
   nstderr <- dup stdError
   (pipeOutputRead,pipeOutputWrite) <- createPipe
   (pipeErrorRead,pipeErrorWrite)   <- createPipe
   _ <- dupTo pipeOutputWrite stdOutput
   _ <- dupTo pipeErrorWrite stdError
   closeFd pipeOutputWrite
   closeFd pipeErrorWrite

   outputLogV <- newEmptyMVar
   errorLogV  <- newEmptyMVar

   let
      -- strict hGetContents
      hGetContents' h  = hGetContents h >>= \s -> length s `seq` return s

   _ <- forkIO (fdToHandle pipeOutputRead >>= hGetContents' >>= putMVar outputLogV)
   _ <- forkIO (fdToHandle pipeErrorRead  >>= hGetContents' >>= putMVar errorLogV)


   (dflgs,dumps) <- withSystemTempDirectory "ghc-web" $ \tmpdir -> do
      -- write module files
      forM_ files $ \file -> do
         -- FIXME: check that fileName is not relative (e.g., ../../etc/passwd)
         putStrLn ("Writing file: " ++ (tmpdir </> fileName file))
         writeFile (tmpdir </> fileName file) (fileContents file)

      withCurrentDirectory tmpdir $ do
         -- execute ghc
         putStrLn ("Executing GHC")
         runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            let dflags' = (profileFlags prof dflags)
                  { dumpDir = Just tmpdir
                  } `gopt_set` Opt_DumpToFile
            void $ setSessionDynFlags dflags'
            target <- guessTarget (fileName (head files)) Nothing
            setTargets [target]
            void $ load LoadAllTargets

            -- read generated files
            df <- getSessionDynFlags
            gd <- liftIO $ readIORef (generatedDumps df)
            dups <- forM (Set.toList gd) $ \p -> do
               -- strip "$tmpdir/./" from the path
               let p' = case drop (length tmpdir) p of
                           x | "/./" `isPrefixOf` x -> drop 3 x
                             | "/" `isPrefixOf` x   -> drop 1 x
                             | otherwise            -> x
               File p' <$> liftIO (readFile p)

            return (dflags',dups)

   -- restore stdError/stdOutput (close pipe write-end)
   _ <- dupTo nstdout stdOutput
   _ <- dupTo nstderr stdError
   closeFd nstderr
   closeFd nstdout

   -- wait for the threads
   outputLog <- takeMVar outputLogV
   errorLog  <- takeMVar errorLogV

   return $ Compilation dflgs files errorLog outputLog dumps



css :: Response
css = toResponseBS (C.pack "text/css") (L.fromStrict $(embedFile "src/style.css"))

-- | Template of all pages
appTemplate :: String -> Html -> Html
appTemplate title bdy = docTypeHtml $ do
   H.head $ do
      H.title (toHtml "GHC Web")
      H.meta ! A.httpEquiv (toValue "Content-Type")
             ! A.content   (toValue "text/html;charset=utf-8")
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
      bdy

-- | Welcoming screen
showWelcome :: [File] -> [CompilationProfile] -> Html
showWelcome files profs = do
   H.p (toHtml "This is a GHC Web frontend. It will help you debug your program and/or GHC.")
   H.p (toHtml "The following files are considered:")
   H.ul $ forM_ files $ \file -> do
      H.li (toHtml (fileName file))
      H.div $ toHtml
         $ formatHtmlBlock defaultFormatOpts
         $ highlightAs "haskell" (fileContents file)
   H.p (toHtml "Now you can compile your files with the profile you want:")
   H.table $ forM_ (profs `zip` [0..]) $ \(prof,(i::Int)) -> do
      H.tr $ do
         H.td $ H.a (toHtml (profileName prof)) ! A.href (toValue ("/compilation/"++show i))
         H.td $ toHtml (profileDesc prof)

showCompilation :: Int -> Compilation -> Html
showCompilation i comp = do
   H.h1 (toHtml "GHC configuration used for this build")
   showDynFlags (compilFlags comp)
   H.h1 (toHtml "Analyse")
   toHtml "Splitted logs sorted by date"
   H.ul $ do
      forM_ (compilSources comp) $ \f -> H.li $ do
         H.a (toHtml (toHtml (fileName f)))
            ! A.href (toValue ("/compilation/"++show i++"/sorted?file="++fileName f))
      H.li $ H.a (toHtml "All files")
         ! A.href (toValue ("/compilation/"++show i ++"/sorted"))

   toHtml "Raw logs"
   H.ul $ do
      forM_ (compilSources comp) $ \f -> H.li $ do
         H.a (toHtml (toHtml (fileName f)))
            ! A.href (toValue ("/compilation/"++show i++"/all?file="++fileName f))
      H.li $ H.a (toHtml "All files")
         ! A.href (toValue ("/compilation/"++show i++"/all"))


showAll :: Maybe String -> Compilation -> Html
showAll fileFilter comp = do
   showFile fileFilter (File "stdout" (compilStdOut comp))
   showFile fileFilter (File "stderr" (compilStdErr comp))
   traverse_ (showFile fileFilter) (compilDumps comp)

showDynFlags :: DynFlags -> Html
showDynFlags dflags = do
   let showFlagEnum :: (Show a, Enum a) => String -> (a -> Bool) -> Html
       showFlagEnum lbl test = do
         H.td $ do
            H.label (toHtml lbl)
            H.br
            H.select (forM_ (enumFrom (toEnum 0)) $ \opt ->
               H.option (toHtml $ show opt)
                     ! (if not (test opt)
                           then A.style (toValue "color:red")
                           else mempty)
                     ! A.value (toValue (show (fromEnum opt)))
               ) ! (A.size (toValue "12"))


   H.table $ H.tr $ do
      showFlagEnum "General flags:" (`gopt` dflags)
      showFlagEnum "Dump flags:" (`dopt` dflags)
      showFlagEnum "Warning flags:" (`wopt` dflags)
      showFlagEnum "Extensions:" (`xopt` dflags)

      H.tr $ H.td $ do
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


showFile :: Maybe String -> File -> Html
showFile fileFilter file = do
   let bypass = case fileFilter of
         Nothing -> False
         Just f  -> dropExtension f /= dropExtension (fileName file)

   unless bypass $ do
      H.h3 (toHtml (fileName file))
      case fileName file of
         "stdout" -> H.pre (toHtml (fileContents file))
         "stderr" -> H.pre (toHtml (fileContents file))
         _        -> forM_ (parseBlocks file) showBlock

showSortedBlocks :: Maybe String -> Compilation -> Html
showSortedBlocks fileFilter comp = do
   let dumps  = compilDumps comp
       ff     = fmap dropExtension fileFilter
   let blocks = [ b | file <- dumps
                    , Just (dropExtension (fileName file)) == ff
                    , b    <- parseBlocks file
                    ]
   forM_ (sortOn blockDate blocks) showBlock

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
   | ".dump-asm"            `isPrefixOf ` ext = "nasm"
   | ".dump-cmm"            `isPrefixOf ` ext = "c"
   | ".dump-opt-cmm"        `isPrefixOf ` ext = "c"
   | ".dump-ds"             `isPrefixOf ` ext = "haskell"
   | ".dump-occur"          `isPrefixOf ` ext = "haskell"
   | ".dump-stranal"        `isPrefixOf ` ext = "haskell"
   | ".dump-spec"           `isPrefixOf ` ext = "haskell"
   | ".dump-cse"            `isPrefixOf ` ext = "haskell"
   | ".dump-call-arity"     `isPrefixOf ` ext = "haskell"
   | ".dump-worker-wrapper" `isPrefixOf ` ext = "haskell"
   | ".dump-parsed"         `isPrefixOf ` ext = "haskell"
   | ".dump-prep"           `isPrefixOf ` ext = "haskell"
   | ".dump-stg"            `isPrefixOf ` ext = "haskell"
   | ".dump-simpl"          `isPrefixOf ` ext = "haskell"
   | ".dump-foreign"        `isPrefixOf ` ext = if "header file" `isSuffixOf` name
                                                   then "c"
                                                   else "haskell"
   | otherwise                               = ""
   where
      ext = takeExtension pth
   
   


data Block = Block
   { blockFile     :: String
   , blockName     :: String
   , blockDate     :: Maybe UTCTime
   , blockContents :: String
   }

parseBlocks :: File -> [Block]
parseBlocks file = case runParser blocks (fileName file) (fileContents file) of
      Right x -> x
      Left e  -> [Block (fileName file) "Whole file"
                  Nothing
                  (fileContents file ++ "\n\n==== Parse error ====\n" ++ show e)]
   where
      blockMark = do
         void eol
         void $ count 20 (char '=')

      blockHead = do
         blockMark
         void $ char ' '
         name <- init <$> manyTill anyChar (char '=')
         void $ count 19 (char '=')
         void eol
         dateStr <- lookAhead (manyTill anyChar eol)
         -- date isn't always present (e.g., typechecker dumps)
         date <- if "UTC" `isSuffixOf` dateStr
            then do
               void (manyTill anyChar eol)
               void eol
               return (Just (read dateStr))
            else return Nothing
         return (name,date)

      block = do
         (name,date) <- blockHead
         contents <- manyTill anyChar (try (lookAhead blockMark) <|> eof)
         return (Block (fileName file) name date contents)

      blocks :: Parser [Block]
      blocks = many block

