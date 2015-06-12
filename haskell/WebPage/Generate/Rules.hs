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
  -- buildSitemap

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
  match "pages/*.html" $ do
    compileToHtml takeBaseName getResourceBody
  match "pages/*.md" $ do
    compileToHtml takeBaseName myPandocCompiler
  match ("projects/**.html" .||. "teaching/**.html") $ do
    compileToHtml dropExtension getResourceBody
  match ("projects/**.md" .||. "teaching/**.md") $ do
    compileToHtml dropExtension myPandocCompiler
  match ("projects/**.pdf" .||. "teaching/**.pdf" .||. "teaching/**.png" .||. "teaching/**.hs" .||. "teaching/**.v" .||. "teaching/**.pl") $ do
    route   idRoute
    compile copyFileCompiler

compileToHtml :: (FilePath -> FilePath) -> Compiler (Item String) -> Rules ()
compileToHtml base body = do
  route (customRoute (flip addExtension "html" . base . toFilePath))
  compile $ do
    path <- fmap toFilePath getUnderlying
    body >>= mainTemplate (takeBaseName path)

myPandocCompiler = pandocCompilerWith
  defaultHakyllReaderOptions {
    -- the citations extension clashes with example lists, which I use for references
    readerExtensions = delete Ext_citations (readerExtensions defaultHakyllReaderOptions)
  }
  defaultHakyllWriterOptions {
    writerHtml5 = True
  }
