{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (mappend)
import Hakyll

--import CV.Paper
--import CV.PaperDB

config = defaultConfiguration {
  destinationDirectory = "/Users/walkie/Desktop/Temp/",
  providerDirectory = ".."
}

compileTemplates = match "templates/*" (compile templateCompiler)

mainTemplate = loadAndApplyTemplate "templates/main.html"
postTemplate = loadAndApplyTemplate "templates/post.html"

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
        >>= mainTemplate postCtx
        >>= postTemplate postCtx
        >>= relativizeUrls

processHome = 
  match "index.html" $ do
    route idRoute
    compile $ getResourceBody >>= mainTemplate defaultContext >>= relativizeUrls

main = hakyllWith config (compileTemplates >> processHome)

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext
