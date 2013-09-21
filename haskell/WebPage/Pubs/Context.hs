{-# LANGUAGE OverloadedStrings #-}

module WebPage.Pubs.Context where

import Data.Monoid ((<>))

import WebPage.Generate.Base
import WebPage.Pubs.Paper
import WebPage.Pubs.Database
import WebPage.Pubs.ToHtml

import Hakyll


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
    abs = "papers/" ++ _key p ++ ".abstract.txt"

-- | Build a list field of publications.
pubListField :: String -> [Paper] -> Context String
pubListField id = listField id baseContext . mapM (makeItem . pubStr)

-- | Build a context containing many fields related to publications.
getPubContext :: Compiler (Context String)
getPubContext = do
    pdfs <- loadAll "papers/*.pdf"
    txts <- loadAll "papers/*.abstract.txt"
    let pubs = map (addAbstract txts . linkPdf pdfs) allPubs
    return $ pubListField "pubs"     pubs
          <> pubListField "journals" (ofKind Journal pubs)
          <> pubListField "chapters" (ofKind Chapter pubs)
          <> pubListField "theses"   (ofKind Thesis pubs)
          <> pubListField "conferences" (ofKinds [Conference,Workshop] pubs)
