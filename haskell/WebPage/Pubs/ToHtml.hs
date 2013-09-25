{-# LANGUAGE OverloadedStrings #-}

module WebPage.Pubs.ToHtml (
  pubStr,
  pubListStr
) where

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)

import Prelude hiding (div,span)

import Text.Blaze.Html5 (AttributeValue,Html,ToMarkup(..),toValue,(!))
import qualified Text.Blaze.Html5 as E
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

import WebPage.Pubs.Paper hiding (paper,venue)


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

bracks :: Html -> Html
bracks h = "[" >> h >> "]"

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

note :: String -> Html
note "Best paper" = div "pub-note pub-best" "Best paper"
note s = div "pub-note" (toMarkup s)

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

codeLink :: String -> Html
codeLink = span "pub-code-link" . (E.a "Code" !) . A.href . toValue

pdfLink :: String -> Html
pdfLink = span "pub-pdf-link" . (E.a "PDF" !) . A.href . toValue

absLink :: String -> Html
absLink key = span (toValue ("pub-abstract-link " ++ key))
            $ E.a "Abstract" ! A.href (toValue script)
  where script = "javascript:toggleAbstract('" ++ key ++ "');"

abstract :: String -> String -> Html
abstract key txt = div (toValue ("pub-abstract " ++ key)) (preEscapedToMarkup txt)

links :: String -> Maybe String -> Maybe String -> Maybe String -> Html
links k a p c = box $ catMaybes
    [fmap (const (absLink k)) a, fmap pdfLink p, fmap codeLink c]
  where box [] = ""
        box hs = span "pub-links" ((bracks . mconcat . intersperse ", ") hs)

paper :: Paper -> Html
paper p = div "pub-block" $ do
    E.a ! A.id (toValue key) $ ""
    div "pub-title"   $ toMarkup (_title p)
    div "pub-authors" $ authors (_authors p)
    div "pub-details" $ details p
    maybe "" note (_note p)
    links key (_abstract p) (_pdfLink p) (_codeLink p)
    maybe "" (abstract key) (_abstract p)
  where key = _key p

pubItem :: Paper -> Html
pubItem = (E.li ! A.class_ "pub-item") . paper

pubList :: [Paper] -> Html
pubList = (E.ol ! A.class_ "pub-list") . mconcat . map pubItem
