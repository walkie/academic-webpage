{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative (empty)
import Data.Monoid         ((<>))
import Hakyll hiding (metadataField)

import CV.PaperDB
import CV.ToHtml


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

mainTemplate :: Context String -> Item String -> Compiler (Item String)
mainTemplate context item = applyAsTemplate context item
    >>= loadAndApplyTemplate "templates/main.html" context

getNewsContext :: Compiler (Context String)
getNewsContext = do
  news <- recentFirst =<< loadAll "news/*"
  return $ listField "news" dateContext (return news) <> mainContext

onPage :: String -> Context String
onPage p = constField ("on" ++ p) ""


-- * Rules

compileTemplates :: Rules ()
compileTemplates =
  match "templates/*" $
    compile templateCompiler

compileMarkdown :: Rules ()
compileMarkdown =
  match ("**/*.md" .&&. complement "pages/*.md") $
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
      let context = constField "research" research
                 <> constField "teaching" teaching
                 <> onPage "Home"
                 <> newsContext
      getResourceBody >>= mainTemplate context

buildTeaching :: Rules ()
buildTeaching =
  match "pages/teaching.md" $ do
    route (constRoute "teaching.html")
    compile $ pandocCompiler
      >>= mainTemplate (onPage "Teaching" <> mainContext)

buildNews :: Rules ()
buildNews =
  match "pages/news.html" $ do
    route (constRoute "news.html")
    compile $ do
      newsContext <- getNewsContext
      getResourceBody
        >>= mainTemplate (onPage "News" <> newsContext)

buildPubs :: Rules ()
buildPubs =
  match "pages/publications.html" $ do
    route (constRoute "publications.html")
    compile $ do
      let context = listField "pubs" mainContext pubItems <> mainContext
      getResourceBody
        >>= mainTemplate (onPage "Pubs" <> context)
  where pubItems = mapM (makeItem . paperString) pubs
    
    

-- * Main

main = hakyllWith config $ do
  compileTemplates
  compileMarkdown
  compileCSS
  copyImages
  buildHome
  buildTeaching
  buildNews
  buildPubs
