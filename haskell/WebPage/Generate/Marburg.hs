{-# LANGUAGE OverloadedStrings #-}

-- | Build current teaching pages for hosting at Marburg.
module WebPage.Generate.Marburg where

import System.FilePath

import Hakyll

import WebPage.Generate.Base
import WebPage.Generate.Context
import WebPage.Generate.Rules

marburgConfig = defaultConfiguration {
  destinationDirectory = "build/marburg/site",
  storeDirectory       = "build/marburg/cache",
  tmpDirectory         = "build/marburg/cache/tmp",
  deployCommand = "rsync -av build/marburg/site/ $MARBURG_STAFF:public_html"
}

marburgRules = do
  compileTemplates
  compileMarkdown
  compileCSS
  copyFiles
  buildMarburgTeaching

buildMarburgTeaching =
  match "teaching/cs609-wi14/**" $ do
    route (customRoute (flip addExtension "html" . dropExtension . toFilePath))
    compilePage simpleTemplate
