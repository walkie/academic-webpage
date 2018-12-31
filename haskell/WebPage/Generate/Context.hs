{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module WebPage.Generate.Context (
  TemplateApplication,
  mainTemplate,
  simpleTemplate
) where

import Data.Monoid ((<>))
import System.FilePath

import Hakyll

import WebPage.Generate.Base
import WebPage.Pubs

import Data.List (find)


-- * Settings

currentNewsLength = 3


-- * Applying Templates

type TemplateApplication = String -> Item String -> Compiler (Item String)

-- | Apply a template to a page of a given name.
applyTemplateTo :: Identifier -> TemplateApplication
applyTemplateTo template page item = do
    context <- fmap (onPage <>) getContext
    applyAsTemplate context item
      >>= loadAndApplyTemplate template context
      >>= relativizeUrls
  where onPage = constField ("on-" ++ page) ""

-- | Apply the main template to a page of a given name.
mainTemplate :: TemplateApplication
mainTemplate = applyTemplateTo "templates/main.html"

-- | Apply the simple template to a page of a given name.
simpleTemplate :: TemplateApplication
simpleTemplate = applyTemplateTo "templates/simple.html"


-- * Extended Context

-- | The complete context.
getContext :: Compiler (Context String)
getContext = do
  pubContext  <- getPubContext
  fileContext <- getBlurbContext
  newsContext <- getNewsContext
  return (fileContext <> pubContext <> newsContext <> baseContext)


-- ** Blurb context

-- | Makes the contents of the blurbs directory available as template fields.
getBlurbContext :: Compiler (Context String)
getBlurbContext = do
    loadAll "blurbs/*"
      >>= return . foldr (<>) baseContext . map item
  where item (Item id body) = constField (takeBaseName (toFilePath id)) body


-- ** News context

-- | Add news items to context as a list.
getNewsContext :: Compiler (Context String)
getNewsContext = do
    page <- getUnderlying
    let news = if page == "pages/index.html"
               then fmap (take currentNewsLength) allNews
               else allNews
    return (listField "news" baseContext news)
  where
    allNews = loadAll "news/*" >>= recentFirst


-- ** Publication context

-- | Adds the PDF link if the file is present.
linkPdf :: String -> [Item CopyFile] -> Paper -> Paper
linkPdf pre fs p
    | Just _ <- lookupItem source fs = p `setPdfLink` target
    | otherwise = p
  where
    source = pre ++ show (_year p) ++ "/" ++ _key p ++ ".pdf"
    target = "/" ++ pre ++ _key p ++ ".pdf"

-- | Add the abstract if the corresponding file is present.
addAbstract :: String -> [Item String] -> Paper -> Paper
addAbstract pre fs p
    | Just i <- lookupItem source fs = p `setAbstract` itemBody i
    | otherwise = p
  where
    source = pre ++ show (_year p) ++ "/" ++ _key p ++ ".abstract.md"

-- | Adds the PDF link and abstract if the corresponding files are present.
addAbstractPdf :: String -> [Item String] -> [Item CopyFile] -> Paper -> Paper
addAbstractPdf pre txts pdfs = addAbstract pre txts . linkPdf pre pdfs

-- | Build a list field of publications.
pubListField :: String -> [Paper] -> Context String
pubListField id = listField id baseContext . mapM (makeItem . pubStr)

-- | Create a field for a publication.
pubFields :: Paper -> Context a
pubFields p = constField (_key p) (pubStr p)

-- | Build a context containing many fields related to publications.
getPubContext :: Compiler (Context String)
getPubContext = do
    pubPdfs <- loadAll "papers/**.pdf"
    pubTxts <- loadAll "papers/**.abstract.md"
    sPdfs <- loadAll "student-theses/*.pdf"
    sTxts <- loadAll "student-theses/*.abstract.md"
    let pubs = map (addAbstractPdf "papers/" pubTxts pubPdfs) allPubs
    let students = map (addAbstractPdf "student-theses/" sTxts sPdfs) studentTheses
    let pubListContext =
             pubListField "pubs"     pubs
          <> pubListField "journals" (ofKind Journal pubs)
          <> pubListField "chapters" (ofKind Chapter pubs)
          <> pubListField "theses"   (ofKind Thesis pubs)
          <> pubListField "conferences" (ofKinds [Conference,Workshop] pubs)
          <> pubListField "student-theses" students
    return $ foldr (<>) pubListContext (map pubFields pubs)
