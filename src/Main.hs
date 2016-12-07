{-# LANGUAGE LambdaCase, TemplateHaskell #-}

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

main :: IO ()
main = withSocketsDo $ do

   opts <- getOptions

   dumps <- createDumpFiles
      [ File "Main.hs"
         "main :: IO ()\n\
         \main = putStrLn \"Hey!\""
      ]

   let conf = nullConf {port = optport opts}

   putStrLn (printf "Starting Web server at localhost:%d" (port conf))
   simpleHTTP conf $ msum
      [ 
        -- CSS 
        dir "css" $ dir "style.css" $ ok css

        -- Show welcome screen
      , nullDir >> (ok . toResponse . appTemplate "Welcome" $ showWelcome dumps)

        -- sorted blocks by date
      , dir "sorted" $ (ok . toResponse . appTemplate "Sorted blocks" $ showSortedBlocks dumps)

      ]


data File = File
   { fileName     :: String
   , fileContents :: String
   } deriving (Show)

-- | Compile Haskell files and return dump files
createDumpFiles :: [File] -> IO [File]
createDumpFiles files = do
   case files of
      []     -> return []
      (x:_) -> do
         withSystemTempDirectory "ghc-web" $ \tmpdir -> do
            -- write module files
            forM_ files $ \file -> do
               -- FIXME: check that fileName is not relative (e.g., ../../etc/passwd)
               writeFile (tmpdir </> fileName file) (fileContents file)

            -- execute ghc
            runGhc (Just libdir) $ do
               dflags <- getSessionDynFlags
               let dflags' = dflags
                     { verbosity = 5
                     , dumpDir = Just tmpdir
                     } `gopt_set` Opt_DumpToFile
               void $ setSessionDynFlags dflags'
               target <- guessTarget (tmpdir </> fileName x) Nothing
               setTargets [target]
               void $ load LoadAllTargets

               -- read generated files
               df <- getSessionDynFlags 
               gd <- liftIO $ readIORef (generatedDumps df)
               forM (Set.toList gd) $ \p -> File p <$> liftIO (readFile p)



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
showWelcome :: [File] -> Html
showWelcome dumps = do
   H.h2 (toHtml "GHC Web")
   traverse_ showFile dumps

showFile :: File -> Html
showFile file = do
   H.h3 (toHtml (takeFileName (fileName file)))
   forM_ (parseBlocks file) showBlock

showSortedBlocks :: [File] -> Html
showSortedBlocks dumps = do
   H.h2 (toHtml "GHC Web")
   let blocks = [ b | file <- dumps
                    , b    <- parseBlocks file
                    ]
   forM_ (sortOn blockDate blocks) showBlock

showBlock :: Block -> Html
showBlock block = do
   H.h4 (toHtml (blockName block))
   H.div (toHtml (blockDate block))
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
   , blockDate     :: String -- UTCTime
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
         date <- manyTill anyChar eol
         void eol
         return (name,date)

      block = do
         (name,date) <- blockHead
         contents <- manyTill anyChar (try (lookAhead blockMark) <|> eof)
         return (Block (fileName file) name date contents)

      blocks :: Parser [Block]
      blocks = many block

