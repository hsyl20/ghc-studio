{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}


import CmdLine (Options(..), getOptions)
import Pipeline
import Profiles

import Haskus.Web.Server
import Haskus.Web.Html
import Haskus.Web.Page
import Haskus.Web.Files (serveFiles)

import Control.Monad

main :: IO ()
main = do

   opts <- getOptions

   let infiles = optfiles opts
   (logs, flgs) <- compileFiles infiles (head defaultProfiles)

   let conf = nullConf {port = optport opts}
   putStrLn ("Starting Web server at localhost:" <> show (port conf))
   simpleHTTP conf $ msum
      [ serveFiles
      , nullDir >> do
         tmplMain do
            div_ do
               p_ (toHtml (show logs))

      ]
      
tmplMain :: Html () -> ServerPartT IO Response
tmplMain bdy = okResponsePage opts bdy 
   where
      opts = HtmlPageOpts
            { pageTitle      = "Haskus Studio" 
            , pageIsWebApp   = True
            , pageOpenGraph  = Nothing
            , pageThemeColor = Nothing
            , pageCss        = [ "/style/style.css"
                               ]
            , pageJquery     = True
            , pageScripts    = []
            , pageManifest   = Just "/manifest.json"
            , pageIcon       = Just "/images/logo-32.png"
            , pageCustomHead = pure ()
            }
