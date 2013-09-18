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

config :: Configuration
config = defaultConfiguration { destinationDirectory = "_site" }


-- * Helper Functions

mainTemplate :: Context a -> Item a -> Compiler (Item String)
mainTemplate = loadAndApplyTemplate "templates/main.html"

getNewsContext :: Compiler (Context String)
getNewsContext = do
  news <- recentFirst =<< loadAll "news/*"
  return $ listField "news" dateContext (return news) <> mainContext


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
      
      research <- loadBody "research/overview.md"
      teaching <- loadBody "teaching/current.md"
      newsContext <- getNewsContext
      let homeContext = constField "research" research
                     <> constField "teaching" teaching
                     <> constField "onHome" ""
                     <> newsContext
      
      getResourceBody 
        >>= applyAsTemplate homeContext
        >>= mainTemplate homeContext

buildTeaching :: Rules ()
buildTeaching =
  create ["teaching.html"] $ do
    route idRoute
    compile $ do
      loadBody "pages/teaching.md"
        >>= makeItem
        >>= mainTemplate mainContext

buildNews :: Rules ()
buildNews =
  match "pages/news.html" $ do
    route (constRoute "news.html")
    compile $ do
      newsContext <- getNewsContext
      getResourceBody
        >>= applyAsTemplate newsContext
        >>= mainTemplate newsContext
    

-- * Main

main = hakyllWith config $ do
  compileTemplates
  compileMarkdown
  compileCSS
  copyImages
  buildHome
  buildTeaching
  buildNews
