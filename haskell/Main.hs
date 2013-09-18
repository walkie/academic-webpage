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
     constField "jquery" "//ajax.googleapis.com/ajax/libs/jquery/2.0.3"
  <> defaultContext

dateContext :: Context String
dateContext =
     dateField "date" "%B %e, %Y"
  <> mainContext

mainTemplate :: Context a -> Item a -> Compiler (Item String)
mainTemplate = loadAndApplyTemplate "templates/main.html"

config :: Configuration
config = defaultConfiguration { destinationDirectory = "_site" }


-- * Rules

compileTemplates :: Rules ()
compileTemplates =
  match "templates/*" $
    compile templateCompiler

compileMarkdown :: Rules ()
compileMarkdown =
  match "**/*.md" $
    compile pandocCompiler

compileCSS :: Rules ()
compileCSS = do
  match "css/*.less" $ do
    compile getResourceBody
    less <- makePatternDependency "css/*.less"
    rulesExtraDependencies [less] $ create ["css/all.css"] $ do
      route idRoute
      compile $ loadBody "css/all.less"
        >>= makeItem
        >>= withItemBody (unixFilter "lessc" ["-"])
        >>= return . fmap compressCss

copyImages :: Rules ()
copyImages =
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

buildHome :: Rules ()
buildHome = 
  match "pages/index.html" $ do
    route (constRoute "index.html")
    compile $ do
      
      news     <- recentFirst =<< loadAll "news/*"
      research <- loadBody "research/overview.md"
      teaching <- loadBody "teaching/current.md"
      let homeContext = listField "news" dateContext (return news)
                        <> constField "research" research
                        <> constField "teaching" teaching
                        <> constField "onHome" ""
                        <> mainContext
      
      getResourceBody 
        >>= applyAsTemplate homeContext
        >>= mainTemplate homeContext
        >>= relativizeUrls

buildTeaching :: Rules ()
buildTeaching =
  create ["teaching.html"] $ do
    route idRoute
    compile $ do
      loadBody "pages/teaching.md"
        >>= makeItem
        >>= mainTemplate mainContext
        >>= relativizeUrls


-- * Main

main = hakyllWith config $ do
  compileTemplates
  compileMarkdown
  compileCSS
  copyImages
  buildHome
  buildTeaching
