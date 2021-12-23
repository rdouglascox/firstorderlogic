module DPform.DPform where

import Parsing.PLParser
import Parsing.PLToken1

import qualified Data.Text as T
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import Data.List
import Data.PLprop

import Printing.Printer
import Fol
import qualified DPLL
import DP

-- here we want functions from text to html via conversion functions

parsePL :: String -> Maybe Prop
parsePL = happyParser . alexScanTokens

splitInput :: T.Text -> [String]
splitInput i = map T.unpack (T.splitOn (T.pack ",") i)

dpformHTML :: T.Text -> H.Html
dpformHTML t = do
    let proplist = mapM parsePL (splitInput t)
    case proplist of
        Nothing -> H5.p (H.toHtml "Invalid input. Try again.")
        Just ps -> dodptrace ps
          
dpformHTMLa :: T.Text -> H.Html
dpformHTMLa t = do
    let proplist = mapM parsePL (splitInput t)
    case proplist of
        Nothing -> H5.p (H.toHtml "Invalid input. Try again.")
        Just ps -> dodptrace (toarg ps)

toarg :: [Prop] -> [Prop]
toarg xs = init xs ++ [Negation $ last xs]