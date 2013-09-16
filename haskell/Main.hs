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
     constField "bootstrap" "//netdna.bootstrapcdn.com/bootstrap/3.0.0"
  <> constField "jquery"    "//ajax.googleapis.com/ajax/libs/jquery/2.0.3"
  <> defaultContext

postContext :: Context String
postContext =
     dateField "date" "%B %e, %Y"
  <> mainContext

mainTemplate, postTemplate :: Context a -> Item a -> Compiler (Item String)
mainTemplate = loadAndApplyTemplate "templates/main.html"
postTemplate = loadAndApplyTemplate "templates/post.html"

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

processPosts :: Rules ()
processPosts =
  match "posts/*" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
        >>= mainTemplate mainContext
        >>= postTemplate postContext
        >>= relativizeUrls

processHome :: Rules ()
processHome = 
  match "pages/index.html" $ do
    route (constRoute "index.html")
    compile $ getResourceBody 
        >>= applyAsTemplate mainContext
        >>= mainTemplate mainContext
        >>= relativizeUrls


-- * Main

main = hakyllWith config $ do
  compileTemplates
  processHome
