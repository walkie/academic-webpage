
module WebPage.Generate.Base where

import Data.List (find)
import Data.Monoid ((<>))
import System.FilePath

import Hakyll


-- * Basic configuration

config :: Configuration
config = defaultConfiguration {
  destinationDirectory = "build/main/site",
  storeDirectory       = "build/main/cache",
  tmpDirectory         = "build/main/cache/tmp",
  deployCommand = "rsync -av build/main/site/ $ACCESS:public_html"
}

baseContext :: Context String
baseContext =
       dateField  "date"   "%B %e, %Y"
    <> constField "jquery" "//ajax.googleapis.com/ajax/libs/jquery/2.0.3"
    <> defaultContext


-- * Utility functions

-- | Lookup an item based on its file path.
lookupItem :: FilePath -> [Item a] -> Maybe (Item a)
lookupItem path = find ((fromFilePath path ==) . itemIdentifier)
