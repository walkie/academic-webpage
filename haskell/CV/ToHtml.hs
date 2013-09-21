{-# LANGUAGE OverloadedStrings #-}

module CV.ToHtml (pubStr,pubListStr) where

import Data.Monoid (mconcat)

import Prelude hiding (div,span)

import Text.Blaze.Html5 (AttributeValue,Html,ToMarkup(..),toValue,(!))
import qualified Text.Blaze.Html5 as E
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

import CV.Paper hiding (paper,venue)


-- * Exported functions

pubStr :: Paper -> String
pubStr = renderHtml . paper

pubListStr :: [Paper] -> String
pubListStr = renderHtml . pubList


-- * Internal functions

comma :: Html -> Html
comma = (>> ", ")

parens :: Html -> Html
parens h = "(" >> h >> ")"

pre :: Html -> Html -> Html
pre = (>>)

fromTo :: ToMarkup a => a -> a -> Html
fromTo a b = toMarkup a >> "–" >> toMarkup b

opt' :: (a -> Html) -> (Html -> Html) -> Maybe a -> Html
opt' to f (Just a) = f (to a)
opt' _  _ Nothing  = ""

opt :: ToMarkup a => (Html -> Html) -> Maybe a -> Html
opt = opt' toMarkup

div :: AttributeValue -> Html -> Html
div a = E.div ! A.class_ a

span :: AttributeValue -> Html -> Html
span a = E.span ! A.class_ a

asList :: [Html] -> Html
asList []      = ""
asList [a]     = a
asList [a,b]   = a >> " and " >> b
asList [a,b,c] = comma a >> comma b >> "and " >> c
asList (a:as)  = comma a >> asList as

author :: Author -> Html
author (Author f l) = toMarkup (f ++ " " ++ l)

authors :: [Author] -> Html
authors = asList . map author

series :: (Name, Int) -> Html
series (n,i) = toMarkup n >> " " >> toMarkup i

pages :: Pages -> Html
pages (Pages     a b) = fromTo a b
pages (PagesIn v a b) = fromTo (col a) (col b)
  where col p = show v ++ ":" ++ show p

year :: Year -> Html
year = span "pub-year" . toMarkup

venue :: Venue -> Html
venue (Venue long short kind _ eds vol num ser) =
  span "pub-venue" $ do
    opt comma kind
    span "pub-venue-name" $ do
      toMarkup long
      opt (pre " " . parens) short
      ", "
    opt' authors (comma . parens . pre "ed. ") eds
    opt' series comma ser
    opt (comma . pre "vol. ") vol
    opt (comma . pre "num. ") num

details :: Paper -> Html
details p = div "pub-details" $ do
  maybe "Draft paper, " venue (_venue p)
  opt' pages comma (_pages p)
  span "pub-year" $ toMarkup (_year p)

paper :: Paper -> Html
paper p = do
  div "pub-title"   $ toMarkup (_title p)
  div "pub-authors" $ authors (_authors p)
  div "pub-details" $ details p

pubItem :: Paper -> Html
pubItem = (E.li ! A.class_ "pub-item") . paper

pubList :: [Paper] -> Html
pubList = (E.ol ! A.class_ "pub-list") . mconcat . map pubItem
