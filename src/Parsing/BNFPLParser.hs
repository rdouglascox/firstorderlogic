module Parsing.BNFPLParser (parse) where

import Grammar.Par
import qualified Grammar.Abs as BNF

import Data.Prop


getrawparse :: String -> BNF.Formula
getrawparse st = case pFormula (myLexer st) of 
  Left s -> error "no parse!"
  Right for -> for 

frombnf :: BNF.Formula -> Formula Prop 
frombnf f = case f of   
  BNF.Top -> Top
  BNF.Bot -> Bot
  BNF.Atom (BNF.Prop p) -> Atom (Prop p)
  BNF.Not for -> Not $ frombnf for
  BNF.And for for' -> And (frombnf for) (frombnf for')
  BNF.Or for for' -> Or (frombnf for) (frombnf for')
  BNF.Imp for for' -> Imp (frombnf for) (frombnf for')
  BNF.Iff for for' -> Iff (frombnf for) (frombnf for')

parse :: String -> Formula Prop
parse = frombnf . getrawparse