{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module WebPage.Generate.Context (
  getContext,
  mainTemplate
) where

import Data.Monoid ((<>))
import System.FilePath

import WebPage.Generate.Base
import WebPage.Pubs

import Hakyll


-- * Exported functions

-- | The complete context.
getContext :: Compiler (Context String)
getContext = do
  pubContext  <- getPubContext
  fileContext <- getBlurbContext
  return (fileContext <> pubContext <> newsContext <> baseContext)


-- | Apply the main template to a page of a given name.
mainTemplate :: String -> Item String -> Compiler (Item String)
mainTemplate page item = do
    context <- fmap (onPage <>) getContext
    applyAsTemplate context item
      >>= loadAndApplyTemplate "templates/main.html" context
      >>= relativizeUrls
  where onPage = constField ("on-" ++ page) ""


-- * Internal functions

-- ** News context

-- | Add news items to context as a list.
newsContext :: Context String
newsContext = listField "news" baseContext (loadAll "news/*" >>= recentFirst)


-- ** File context

-- | Makes the contents of the blurbs directory available as template fields.
getBlurbContext :: Compiler (Context String)
getBlurbContext = do
    loadAll "blurbs/*"
      >>= return . foldr (<>) baseContext . map item
  where item (Item id body) = constField (takeBaseName (toFilePath id)) body


-- ** Publication context

-- | Adds the PDF link if the file is present.
linkPdf :: [Item CopyFile] -> Paper -> Paper
linkPdf fs p
    | Just _ <- lookupItem pdf fs = p `setPdfLink` ("/" ++ pdf)
    | otherwise = p
  where
    pdf = "papers/" ++ _key p ++ ".pdf"

-- | Add the abstract if the corresponding file is present.
addAbstract :: [Item String] -> Paper -> Paper
addAbstract fs p
    | Just i <- lookupItem abs fs = p `setAbstract` itemBody i
    | otherwise = p
  where
    abs = "papers/" ++ _key p ++ ".abstract.md"

-- | Build a list field of publications.
pubListField :: String -> [Paper] -> Context String
pubListField id = listField id baseContext . mapM (makeItem . pubStr)

-- | Create a field for a publications.
pubFields :: Paper -> Context a
pubFields p = constField (_key p) (pubStr p)

-- | Build a context containing many fields related to publications.
getPubContext :: Compiler (Context String)
getPubContext = do
    pdfs <- loadAll "papers/*.pdf"
    txts <- loadAll "papers/*.abstract.md"
    let pubs = map (addAbstract txts . linkPdf pdfs) allPubs
    let pubListContext =
             pubListField "pubs"     pubs
          <> pubListField "journals" (ofKind Journal pubs)
          <> pubListField "chapters" (ofKind Chapter pubs)
          <> pubListField "theses"   (ofKind Thesis pubs)
          <> pubListField "conferences" (ofKinds [Conference,Workshop] pubs)
    return $ foldr (<>) pubListContext (map pubFields pubs)
