{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate)
import Data.Monoid ((<>))
import System.FilePath

import Hakyll

import CV.PaperDB
import CV.ToHtml


config :: Configuration
config = defaultConfiguration { destinationDirectory = "_site" }


-- * Contexts

baseContext :: Context String
baseContext =
       dateField  "date"   "%B %e, %Y"
    <> constField "jquery" "//ajax.googleapis.com/ajax/libs/jquery/2.0.3"
    <> defaultContext

staticContext :: Context String
staticContext =
       listField "news" baseContext (loadAll "news/*" >>= recentFirst)
    <> listField "pubs" baseContext (mapM (makeItem . paperString) pubs)
    <> baseContext

-- | Makes the contents of several directories available as template fields.
--   The content of a file dir/file.ext will be available as $dir-file$.
dynamicContext :: Compiler (Context String)
dynamicContext = loadAll ("blurbs/*" .||. "research/*" .||. "teaching/*")
    >>= return . foldr (<>) staticContext . map item
  where item (Item id body) = constField (name id) body
        name = intercalate "-" . splitDirectories . dropExtension . toFilePath

-- | Apply the main template to a page of a given name.
mainTemplate :: String -> Item String -> Compiler (Item String)
mainTemplate page item = do
    context <- fmap (onPage <>) dynamicContext
    applyAsTemplate context item
      >>= loadAndApplyTemplate "templates/main.html" context
  where onPage = constField ("on-" ++ page) ""


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

copyFiles :: Rules ()
copyFiles =
  match ("images/*" .||. "js/*") $ do
    route   idRoute
    compile copyFileCompiler

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
    

-- * Main

main = hakyllWith config $ do
  compileTemplates
  compileMarkdown
  compileCSS
  copyFiles
  buildPages
