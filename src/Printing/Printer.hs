module Printing.Printer (printfol) where

import Data.Prop

printfol :: Formula Fol -> String
printfol = printFormula 

printFol :: Fol -> String 
printFol (R st ts) = st ++ printTerms ts

printTerm :: Term -> String 
printTerm (Var st) = st 
printTerm (Fn st ts) = st ++ printTerms ts 

printTerms :: [Term] -> String 
printTerms = concatMap printTerm 

printFormula :: Formula Fol -> String 
printFormula f = case f of  
  Bot -> "False"
  Top -> "True"
  Atom fol -> printFol fol
  Not for -> "~" ++ printFormula for
  And for for' -> "(" ++ printFormula for ++ "&" ++ printFormula for' ++ ")"
  Or for for' -> "(" ++ printFormula for ++ "v" ++ printFormula for' ++ ")"
  Imp for for' -> "(" ++ printFormula for ++ "->" ++ printFormula for' ++ ")"
  Iff for for' -> "(" ++ printFormula for ++ "<->" ++ printFormula for' ++ ")"
  Forall s for -> "@" ++ s ++ printFormula for
  Exists s for -> "#" ++ s ++ printFormula for
