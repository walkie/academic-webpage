{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative (empty)
import Data.Monoid         ((<>))
import Hakyll hiding (metadataField)

--import CV.Paper
--import CV.PaperDB


-- * Configuration

mainContext :: Context String
mainContext =
     constField "jquery"    "//ajax.googleapis.com/ajax/libs/jquery/2.0.3"
  <> defaultContext

dateContext :: Context String
dateContext =
     dateField "date" "%B %e, %Y"
  <> mainContext

mainTemplate :: Context a -> Item a -> Compiler (Item String)
mainTemplate = loadAndApplyTemplate "templates/main.html"

config :: Configuration
config = defaultConfiguration {
  destinationDirectory = "../_site",
  providerDirectory = ".."
}


-- * Rules

compileTemplates :: Rules ()
compileTemplates =
  match "templates/*" $
    compile templateCompiler

copyImages :: Rules ()
copyImages =
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

copyCSS :: Rules ()
copyCSS =
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

processNews :: Rules ()
processNews =
  match "news/*" $
    compile pandocCompiler

processText :: Rules ()
processText =
  match "text/*" $
    compile pandocCompiler

processHome :: Rules ()
processHome = 
  match "pages/index.html" $ do
    route (constRoute "index.html")
    compile $ do
      
      news     <- recentFirst =<< loadAll "news/*"
      overview <- loadBody "text/research-overview.md"
      let homeContext = listField "news" dateContext (return news)
                        <> constField "overview" overview
                        <> mainContext
      
      getResourceBody 
        >>= applyAsTemplate homeContext
        >>= mainTemplate homeContext
        >>= relativizeUrls


-- * Main

main = hakyllWith config $ do
  compileTemplates
  copyImages
  copyCSS
  processNews
  processText
  processHome
