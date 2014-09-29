{-# LANGUAGE OverloadedStrings #-}

module WebPage.Generate.Rules where

import Data.Set (delete)
import System.FilePath

import Hakyll
import Text.Pandoc.Options

import WebPage.Generate.Base
import WebPage.Generate.Context
import WebPage.Generate.Sitemap
import WebPage.Pubs


rules :: Rules ()
rules = do
  compileTemplates
  compileMarkdown
  compileCSS
  copyFiles
  copyPapers
  loadAbstracts
  buildPages
  buildSitemap

compileTemplates :: Rules ()
compileTemplates =
  match "templates/*" $
    compile templateCompiler

compileMarkdown :: Rules ()
compileMarkdown =
  match ("blurbs/*.md" .||. "news/*.md") $
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
  match ("images/*" .||. "js/*" .||. "css/*.css") $ do
    route   idRoute
    compile copyFileCompiler

copyPapers :: Rules ()
copyPapers =
  match "papers/*.pdf" $ do
    route   idRoute
    compile copyFileCompiler

loadAbstracts :: Rules ()
loadAbstracts =
  match "papers/*.abstract.md" $
    compile pandocCompiler

buildSitemap :: Rules ()
buildSitemap =
  create ["sitemap.xml"] $ do
    route idRoute
    compileSitemap

buildPages :: Rules ()
buildPages = do
  match "pages/*" $ do
    route (customRoute (flip addExtension "html" . takeBaseName . toFilePath))
    compilePage mainTemplate
  match ("projects/*" .||. "teaching/**") $ do
    route (customRoute (flip addExtension "html" . dropExtension . toFilePath))
    compilePage mainTemplate

compilePage :: TemplateApplication -> Rules ()
compilePage apply = compile $ do 
  path <- fmap toFilePath getUnderlying
  let content = case takeExtension path of
        ".html" -> getResourceBody
        ".txt"  -> getResourceBody
        ".md"   -> myPandocCompiler
        _       -> error ("Unexpected file type: " ++ path)
  content >>= apply (takeBaseName path)

myPandocCompiler = pandocCompilerWith
  defaultHakyllReaderOptions {
    -- the citations extension clashes with example lists, which I use for references
    readerExtensions = delete Ext_citations (readerExtensions defaultHakyllReaderOptions)
  }
  defaultHakyllWriterOptions {
    writerHtml5 = True
  }
