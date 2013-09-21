{-# LANGUAGE OverloadedStrings #-}

module WebPage.Generate.Rules (rules) where

import System.FilePath

import Hakyll

import WebPage.Generate.Base
import WebPage.Generate.Context
import WebPage.Pubs


-- * Exported functions

rules = do
  compileTemplates
  compileMarkdown
  compileCSS
  copyFiles
  loadAbstracts
  buildPages


-- * Internal functions

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

copyFiles :: Rules ()
copyFiles =
  match ("images/*" .||. "js/*" .||. "papers/*.pdf") $ do
    route   idRoute
    compile copyFileCompiler

loadAbstracts :: Rules ()
loadAbstracts =
  match "papers/*.abstract.txt" $
    compile getResourceBody

buildPages :: Rules()
buildPages =
  match "pages/*" $ do
    route (customRoute (flip addExtension "html" . takeBaseName . toFilePath))
    compile $ do 
      path <- fmap toFilePath getUnderlying
      let content = case takeExtension path of
            ".html" -> getResourceBody
            ".md"   -> pandocCompiler
            _       -> error ("Unexpected file type: " ++ path)
      content >>= mainTemplate (takeBaseName path)
