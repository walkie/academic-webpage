{-# LANGUAGE OverloadedStrings #-}

module CV.ToHtml where

import Prelude hiding (div,span)

import Text.Blaze.Html5 (AttributeValue,Html,toMarkup,toValue,(!))
import qualified Text.Blaze.Html5 as E
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

import CV.Paper hiding (paper,venue)

div :: AttributeValue -> Html -> Html
div a = E.div ! A.class_ a

span :: AttributeValue -> Html -> Html
span a = E.span ! A.class_ a

asList :: [Html] -> Html
asList []      = ""
asList [a]     = a
asList [a,b]   = a >> " and " >> b
asList [a,b,c] = a >> ", " >> b >> ", and " >> c
asList (a:as)  = a >> ", " >> asList as

author :: Author -> Html
author (Author f l) = toMarkup (f ++ " " ++ l)

authors :: [Author] -> Html
authors = div "pub-authors" . asList . map author

title :: Title -> Html
title = div "pub-title" . toMarkup

year :: Year -> Html
year = span "pub-year" . toMarkup

venue :: Venue -> Html
venue _ = "To do"

paper :: Paper -> Html
paper p = do
    title (_title p)
    authors (_authors p)
    div "pub-details" $ do
      maybe "Draft paper" venue (_venue p) >> ", "
      year (_year p) >> "."

paperString :: Paper -> String
paperString = renderHtml . paper
