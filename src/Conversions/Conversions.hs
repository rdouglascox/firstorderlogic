module Conversions.Conversions where

import Parsing.Parser ( parser )
import Parsing.FolParser ( E(Failed, Ok) )
import qualified Data.Text as T
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import Data.List
import Data.Prop

import Printing.Printer
import Fol
import qualified DPLL

import qualified GPL

-- here we want functions from text to html via conversion functions

safeconversions :: T.Text -> (Bool,Bool,Bool,Bool) -> H.Html
safeconversions t x = let pt = parser (T.unpack t) in conversions pt x

conversions :: Formula Fol -> (Bool,Bool,Bool,Bool) -> H.Html
conversions t (n,c,d,p) 
  | n = nnfHTML t <> H5.br <> H5.br <> conversions t (False,c,d,p)
  | c = cnfHTML t <> H5.br <> H5.br <> conversions t (False,False,d,p)
  | d = dnfHTML t <> H5.br <> H5.br <> conversions t (False,False,False,p)
  | p = pnfHTML t 
  | otherwise = H.toHtml ""

nnfHTML :: Formula Fol -> H.Html
nnfHTML t = H.toHtml "Negation Normal Form: " <> H.toHtml (printfol (nnf t))

cnfHTML :: Formula Fol -> H.Html
cnfHTML t = H.toHtml "Conjunctive Normal Form: " <> H.toHtml (printfol (DPLL.cnf t))

dnfHTML :: Formula Fol -> H.Html
dnfHTML t = H.toHtml "Disjunctive Normal Form: " <> H.toHtml (printfol (DPLL.dnf t))

pnfHTML :: Formula Fol -> H.Html
pnfHTML t = H.toHtml "Prenex Normal Form: " <> H.toHtml (printfol (GPL.pnf  t ))



safeconversions' :: T.Text -> (Bool,Bool,Bool,Bool) -> H.Html
safeconversions' t x = let pt = parser (T.unpack t) in conversions' pt x

conversions' :: Formula Fol -> (Bool,Bool,Bool,Bool) -> H.Html
conversions' t (n,c,d,p) 
  | n = nnfHTML' t <> H5.br <> H5.br <> conversions' t (False,c,d,p)
  | c = cnfHTML' t <> H5.br <> H5.br <> conversions' t (False,False,d,p)
  | d = dnfHTML' t <> H5.br <> H5.br <> conversions' t (False,False,False,p)
  | p = pnfHTML' t 
  | otherwise = H.toHtml ""

nnfHTML' :: Formula Fol -> H.Html
nnfHTML' t = H.toHtml "Negation Normal Form: " <> H.toHtml (printFormulaASCII (nnf t))

cnfHTML' :: Formula Fol -> H.Html
cnfHTML' t = H.toHtml "Conjunctive Normal Form: " <> H.toHtml (printFormulaASCII (DPLL.cnf t))

dnfHTML' :: Formula Fol -> H.Html
dnfHTML' t = H.toHtml "Disjunctive Normal Form: " <> H.toHtml (printFormulaASCII (DPLL.dnf t))

pnfHTML' :: Formula Fol -> H.Html
pnfHTML' t = H.toHtml "Prenex Normal Form: " <> H.toHtml (printFormulaASCII (GPL.pnf  t ))