{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Hakyll

--import CV.Paper
--import CV.PaperDB

config = defaultConfiguration {
  destinationDirectory = "/Users/walkie/Desktop/Temp/",
  providerDirectory = ".."
}

mainTemplate = loadAndApplyTemplate "templates/main.html"
postTemplate = loadAndApplyTemplate "templates/post.html"

mainContext = defaultContext
postContext = dateField "date" "%B %e, %Y" <> mainContext

compileTemplates =
  match "templates/*" $ 
    compile templateCompiler

copyImages =
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

copyCSS =
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

processPosts =
  match "posts/*" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
        >>= mainTemplate mainContext
        >>= postTemplate postContext
        >>= relativizeUrls

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
