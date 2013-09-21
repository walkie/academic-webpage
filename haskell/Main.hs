{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate)
import Data.Monoid ((<>))
import System.FilePath

import Hakyll

import WebPage.Generate.Base
import WebPage.Pubs



-- * Contexts

newsContext :: Context String
newsContext = listField "news" baseContext (loadAll "news/*" >>= recentFirst)

-- | Makes the contents of several directories available as template fields.
--   The content of a file dir/file.ext will be available as $dir-file$.
getFileContext :: Compiler (Context String)
getFileContext = do
    loadAll ("misc/*" .||. "research/*" .||. "teaching/*")
      >>= return . foldr (<>) baseContext . map item
  where item (Item id body) = constField (name id) body
        name = intercalate "-" . splitDirectories . dropExtension . toFilePath

getContext :: Compiler (Context String)
getContext = do
  pubContext  <- getPubContext
  fileContext <- getFileContext
  return (fileContext <> pubContext <> newsContext <> baseContext)


-- | Apply the main template to a page of a given name.
mainTemplate :: String -> Item String -> Compiler (Item String)
mainTemplate page item = do
    context <- fmap (onPage <>) getContext
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
    

-- * Main

main = hakyllWith config $ do
  compileTemplates
  compileMarkdown
  compileCSS
  copyFiles
  buildPages
