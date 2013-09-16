{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Hakyll

--import CV.Paper
--import CV.PaperDB

config :: Configuration
config = defaultConfiguration {
  destinationDirectory = "/Users/walkie/Desktop/Temp/",
  providerDirectory = ".."
}

mainTemplate, postTemplate :: Context a -> Item a -> Compiler (Item String)
mainTemplate = loadAndApplyTemplate "templates/main.html"
postTemplate = loadAndApplyTemplate "templates/post.html"

mainContext, postContext :: Context String
mainContext = defaultContext
postContext = dateField "date" "%B %e, %Y" <> mainContext

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
  match "index.html" $ do
    route idRoute
    compile $ getResourceBody 
        >>= applyAsTemplate mainContext
        >>= mainTemplate mainContext
        >>= relativizeUrls

main = hakyllWith config $ do
  compileTemplates
  processHome
