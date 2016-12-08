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
import System.Directory
import Data.Time.Clock


data Compilation = Compilation
   { compilFlags  :: DynFlags
   , compilStdErr :: String
   , compilStdOut :: String
   , compilDumps  :: [File]
   }

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

   comp <- compileFiles infiles

   let conf = nullConf {port = optport opts}
   putStrLn (printf "Starting Web server at localhost:%d" (port conf))
   simpleHTTP conf $ msum
      [ 
        -- CSS 
        dir "css" $ dir "style.css" $ ok css

        -- Show welcome screen
      , nullDir >> (ok . toResponse . appTemplate "Welcome" $ showWelcome infiles [comp])

      , dir "all" $ (ok . toResponse . appTemplate "All results" $ showAll comp)

        -- sorted blocks by date
      , dir "sorted" $ (ok . toResponse . appTemplate "Sorted blocks" $ showSortedBlocks comp)

      ]


data File = File
   { fileName     :: String
   , fileContents :: String
   } deriving (Show)


compileFiles :: [File] -> IO Compilation
compileFiles files = do
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
            let dflags' = dflags
                  { verbosity = 5
                  , dumpDir = Just tmpdir
                  } `gopt_set` Opt_DumpToFile
            void $ setSessionDynFlags dflags'
            target <- guessTarget (fileName (head files)) Nothing
            setTargets [target]
            void $ load LoadAllTargets

            -- read generated files
            df <- getSessionDynFlags
            gd <- liftIO $ readIORef (generatedDumps df)
            dups <- forM (Set.toList gd) $ \p -> File p <$> liftIO (readFile p)

            return (dflags',dups)

   -- restore stdError/stdOutput (close pipe write-end)
   _ <- dupTo nstdout stdOutput
   _ <- dupTo nstderr stdError
   closeFd nstderr
   closeFd nstdout

   -- wait for the threads
   outputLog <- takeMVar outputLogV
   errorLog  <- takeMVar errorLogV

   return $ Compilation dflgs errorLog outputLog dumps



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
      bdy

-- | Welcoming screen
showWelcome :: [File] -> [Compilation] -> Html
showWelcome files comps = do
   H.p (toHtml "This is a GHC Web frontend. It will help you debug your program and/or GHC.")
   H.p (toHtml "The following files are considered:")
   H.ul $ forM_ files $ \file -> do
      H.li (toHtml (fileName file))
      H.div $ toHtml
         $ formatHtmlBlock defaultFormatOpts
         $ highlightAs "haskell" (fileContents file)
   H.p (toHtml "Now you can compile your files with the options you want:")
   forM_ comps $ \comp -> do
      showDynFlags (compilFlags comp)
      H.a (toHtml "All results") ! A.href (toValue "all")
      H.br
      H.a (toHtml "Sorted blocks") ! A.href (toValue "sorted")

showAll :: Compilation -> Html
showAll comp = do
   showFile (File "stdout" (compilStdOut comp))
   showFile (File "stderr" (compilStdErr comp))
   traverse_ showFile (compilDumps comp)

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


showFile :: File -> Html
showFile file = do
   H.h3 (toHtml (takeFileName (fileName file)))
   case fileName file of
      "stdout" -> H.pre (toHtml (fileContents file))
      "stderr" -> H.pre (toHtml (fileContents file))
      _        -> forM_ (parseBlocks file) showBlock

showSortedBlocks :: Compilation -> Html
showSortedBlocks comp = do
   let dumps  = compilDumps comp
   let blocks = [ b | file <- dumps
                    , b    <- parseBlocks file
                    ]
   forM_ (sortOn blockDate blocks) showBlock

showBlock :: Block -> Html
showBlock block = do
   H.h4 (toHtml (blockName block))
   H.div (toHtml (show (blockDate block))) ! A.class_ (toValue "date")
   H.div $ toHtml
      $ formatHtmlBlock defaultFormatOpts
      $ highlightAs (selectFormat (blockFile block) (blockName block)) (blockContents block)

-- | Select highlighting format
selectFormat :: FilePath -> String -> String
selectFormat pth name
   | ".dump-asm"     `isPrefixOf ` ext = "nasm"
   | ".dump-cmm"     `isPrefixOf ` ext = "c"
   | ".dump-opt-cmm" `isPrefixOf ` ext = "c"
   | ".dump-ds"      `isPrefixOf ` ext = "haskell"
   | ".dump-occur"   `isPrefixOf ` ext = "haskell"
   | ".dump-parsed"  `isPrefixOf ` ext = "haskell"
   | ".dump-prep"    `isPrefixOf ` ext = "haskell"
   | ".dump-stg"     `isPrefixOf ` ext = "haskell"
   | ".dump-simpl"   `isPrefixOf ` ext = "haskell"
   | ".dump-foreign" `isPrefixOf ` ext = if "header file" `isSuffixOf` name
                                             then "c"
                                             else "haskell"
   | otherwise                         = ""
   where
      ext = takeExtension pth
   
   


data Block = Block
   { blockFile     :: String
   , blockName     :: String
   , blockDate     :: UTCTime
   , blockContents :: String
   }

parseBlocks :: File -> [Block]
parseBlocks file = case runParser blocks (fileName file) (fileContents file) of
      Right x -> x
      Left e  -> error (show e)
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
         date <- read <$> manyTill anyChar eol
         void eol
         return (name,date)

      block = do
         (name,date) <- blockHead
         contents <- manyTill anyChar (try (lookAhead blockMark) <|> eof)
         return (Block (fileName file) name date contents)

      blocks :: Parser [Block]
      blocks = many block

