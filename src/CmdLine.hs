module CmdLine
   ( Options(..)
   , getOptions
   )
where

import Options.Applicative

data Options = Options
   { optport   :: Int
   , optfiles  :: [String]
   }

options :: Parser Options
options = Options
  <$> option auto (
        long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 8000
     <> help "Use port PORT for the HTTP server"
     )
  <*> some (argument str (metavar "FILES..."))
  

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "GHC Web Frontend"
     <> header "GHC Web Frontend" )
