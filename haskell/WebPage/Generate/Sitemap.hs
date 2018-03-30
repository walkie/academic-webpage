{-# LANGUAGE OverloadedStrings #-}

module WebPage.Generate.Sitemap (
  compileSitemap
) where

import Data.List (isPrefixOf)
import Data.Monoid ((<>))

import Hakyll

import WebPage.Generate.Base


-- * Settings

-- | The site root.
siteRoot = "http://web.engr.oregonstate.edu/~walkiner"

-- | Get the priority of a page.
priority :: String -> Float
priority "/index.html" = 1.0
priority "/publications.html" = 1.0
priority "/legal.html" = 0.1
priority url
  | isPrefixOf "/papers/"   url = 0.8
  | isPrefixOf "/projects/" url = 0.5
  | isPrefixOf "/teaching/" url = 0.3
  | otherwise                   = 0.5

-- | Matches all pages to include.
allPages :: Compiler [Item String]
allPages = loadAll $ "pages/*.html" .||. "pages/*.md"
                .||. "projects/**.html" .||. "projects/**.md"
                .||. "teaching/**.html" .||. "teaching/**.md"

-- | Matches all files to include.
allFiles :: Compiler [Item CopyFile]
allFiles = loadAll "papers/*.pdf"


-- * Interface

-- | Rule to compile the sitemap.
compileSitemap :: Rules ()
compileSitemap = compile $
  makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapContext


-- * Internals

-- | Contains the lists of URLs to include in the sitemap.
sitemapContext :: Context a
sitemapContext = listField "pages" urlContext allPages
              <> listField "files" urlContext allFiles

-- | Defines the location and priority for each URL.
urlContext :: Context a
urlContext = field "location" getFullUrl
          <> field "priority" getPriority

-- | Get the site-relative URL for an item.
getSiteUrl :: Item a -> Compiler String
getSiteUrl (Item k _) = fmap (maybe err toUrl) (getRoute k)
  where err = error $ "Sitemap.getSiteUrl: no route for: " ++ show k

-- | Get the absolute URL for an item.
getFullUrl :: Item a -> Compiler String
getFullUrl = fmap (siteRoot ++) . getSiteUrl

-- | Get the priority of an item.
getPriority :: Item a -> Compiler String
getPriority = fmap (show . priority) . getSiteUrl
