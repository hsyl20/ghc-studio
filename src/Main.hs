{-# LANGUAGE LambdaCase, TemplateHaskell #-}

import CmdLine (Options(..), getOptions)

import Control.Monad
import Text.Printf
import Network.Socket (withSocketsDo)
import Network.HTTP.Base (urlEncode)
import Numeric
import Happstack.Server
import Data.Char (toUpper)
import Data.Maybe
import Data.FileEmbed
import System.IO.Temp
import System.FilePath
import Data.IORef
import Control.Monad.IO.Class
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Vector as V

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
         withSystemTempDirectory "ghc-web" $ \tmpDir -> do
            -- write module files
            forM files $ \file -> do
               -- FIXME: check that fileName is not relative (e.g., ../../etc/passwd)
               writeFile (tmpDir </> fileName file) (fileContents file)

            defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
               -- execute ghc
               runGhc (Just libdir) $ do
                  dflags <- getSessionDynFlags
                  let dflags' = dflags
                        { verbosity = 5
                        , dumpDir = Just tmpDir
                        } `gopt_set` Opt_DumpToFile
                  setSessionDynFlags dflags'
                  target <- guessTarget (tmpDir </> fileName x) Nothing
                  setTargets [target]
                  load LoadAllTargets

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
   H.body $ do
      H.div (toHtml $ "GHC Web " ++ " / " ++ title)
         ! A.class_ (toValue "headtitle")
      bdy

-- | Welcoming screen
showWelcome :: [File] -> Html
showWelcome dumps = do
   H.h2 (toHtml "GHC Web")
   H.div (toHtml (show dumps))
