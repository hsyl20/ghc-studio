module Pipeline where

import Control.Monad
import System.Directory
import System.FilePath
import System.IO.Temp
import Data.IORef

import Profiles

import GHC
import GHC.Paths ( libdir )
import Outputable (SDoc,PprStyle,renderWithStyle,showSDoc,ppr)
import DynFlags

data Log = Log
   { logId       :: !Word
   , logDynFlags :: !DynFlags
   , logReason   :: !WarnReason
   , logSeverity :: !Severity
   , logLocation :: !SrcSpan
   , logStyle    :: !PprStyle
   , logMessage  :: !SDoc
   }

instance Show Log where
   show l = "Log "++ show (logId l) ++ ":"
            ++ "\n\tSeverity: "++ showLogSeverity l
            ++ "\n\tContents: "++ showLogMessage l

showLogSeverity :: Log -> String
showLogSeverity l = case logSeverity l of
   SevOutput      -> "Output"
   SevFatal       -> "Fatal"
   SevInteractive -> "Interactive"
   SevDump        -> "Dump"
   SevInfo        -> "Info"
   SevWarning     -> "Warning"
   SevError       -> "Error"

showLogMessage :: Log -> String
showLogMessage l = showSDoc (logDynFlags l) (logMessage l)

compileFiles :: [FilePath] -> CompilationProfile -> IO ([Log],DynFlags)
compileFiles files prof = do
   logs <- newIORef []
   cnt  <- newIORef 0

   dflgs <- withSystemTempDirectory "haskus-studio" $ \tmpdir -> do
      -- write module files
      forM_ files $ \file -> do
         -- FIXME: check that fileName is not relative (e.g., ../../etc/passwd)
         createDirectoryIfMissing True (tmpdir </> takeDirectory file)
         copyFile file (tmpdir </> file)

      withCurrentDirectory tmpdir $ do
         -- execute ghc
         runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            let logact dfl reason sev srcspan style msg = do
                  lid <- readIORef cnt
                  modifyIORef' logs (Log lid dfl reason sev srcspan style msg :)
                  modifyIORef' cnt (+1) 
                  --log_action dflags dfl reason sev srcspan style msg
                  return ()
            let dflags' = (profileFlags prof dflags)
                  { dumpDir       = Just tmpdir
                  , log_action    = logact
                  }
            void $ setSessionDynFlags dflags'
            target <- guessTarget (head files) Nothing
            setTargets [target]
            void $ load LoadAllTargets

            return dflags'

   logs'    <- reverse <$> readIORef logs
   return (logs',dflgs)
