
module Main where

import System.Environment (getArgs,withArgs)

import Hakyll

import WebPage.Generate
import WebPage.Generate.Marburg

main = do
  args <- getArgs
  case args of
    "marburg" : as -> withArgs as (hakyllWith marburgConfig marburgRules)
    _              -> hakyllWith config rules
