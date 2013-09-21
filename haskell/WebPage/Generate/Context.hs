{-# LANGUAGE OverloadedStrings #-}

module WebPage.Generate.Context (
  getContext,
  mainTemplate
) where

import Data.List (intercalate)
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
  fileContext <- getFileContext
  return (fileContext <> pubContext <> newsContext <> baseContext)


-- | Apply the main template to a page of a given name.
mainTemplate :: String -> Item String -> Compiler (Item String)
mainTemplate page item = do
    context <- fmap (onPage <>) getContext
    applyAsTemplate context item
      >>= loadAndApplyTemplate "templates/main.html" context
  where onPage = constField ("on-" ++ page) ""


-- * Internal functions

-- ** News context

-- | Add news items to context as a list.
newsContext :: Context String
newsContext = listField "news" baseContext (loadAll "news/*" >>= recentFirst)


-- ** File context

-- | Makes the contents of several directories available as template fields.
--   The content of a file dir/file.ext will be available as $dir-file$.
getFileContext :: Compiler (Context String)
getFileContext = do
    loadAll ("misc/*" .||. "research/*" .||. "teaching/*")
      >>= return . foldr (<>) baseContext . map item
  where item (Item id body) = constField (name id) body
        name = intercalate "-" . splitDirectories . dropExtension . toFilePath


-- ** Publication context

-- | Adds the PDF link if the file is present.
linkPdf :: [Item CopyFile] -> Paper -> Paper
linkPdf fs p
    | Just _ <- lookupItem pdf fs = p `setPdfLink` pdf
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

-- | Build a context containing many fields related to publications.
getPubContext :: Compiler (Context String)
getPubContext = do
    pdfs <- loadAll "papers/*.pdf"
    txts <- loadAll "papers/*.abstract.md"
    let pubs = map (addAbstract txts . linkPdf pdfs) allPubs
    return $ pubListField "pubs"     pubs
          <> pubListField "journals" (ofKind Journal pubs)
          <> pubListField "chapters" (ofKind Chapter pubs)
          <> pubListField "theses"   (ofKind Thesis pubs)
          <> pubListField "conferences" (ofKinds [Conference,Workshop] pubs)
