module Printing.Printer (printfol) where

import Data.Prop
import Data.List (intercalate)

printfol :: Formula Fol -> String
printfol = printFormula 

printFol :: Fol -> String 
printFol (R st ts) = st ++ printTerms ts

printTerm :: Term -> String 
printTerm (Var st) = st 
printTerm (Fn st ts) = st ++ "(" ++ printTerms' ts ++ ")"

printTerms :: [Term] -> String 
printTerms = concatMap printTerm 

printTerms' :: [Term] -> String 
printTerms' ts  = intercalate "," $ map printTerm ts

printFormula :: Formula Fol -> String 
printFormula f = case f of  
  Bot -> "⊥"
  Top -> "⊤"
  Atom fol -> printFol fol
  Not for -> "¬" ++ printFormula for
  And for for' -> "(" ++ printFormula for ++ "∧" ++ printFormula for' ++ ")"
  Or for for' -> "(" ++ printFormula for ++ "∨" ++ printFormula for' ++ ")"
  Imp for for' -> "(" ++ printFormula for ++ "→" ++ printFormula for' ++ ")"
  Iff for for' -> "(" ++ printFormula for ++ "⟷" ++ printFormula for' ++ ")"
  Forall s for -> "∀" ++ s ++ printFormula for
  Exists s for -> "∃" ++ s ++ printFormula for
