module Printing.Printer (printfol,printProp,printprops,printFormulaASCII) where

import Data.Prop
import Data.List (intercalate)

printfol :: Formula Fol -> String
printfol = printFormula 

printFol :: Fol -> String 
printFol (R st ts) = st ++ printTerms ts

printTerm :: Term -> String 
printTerm (Var st) = st 
printTerm (Fn st []) = st
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

printProp :: Formula Prop -> String 
printProp f = case f of  
  Bot -> "⊥"
  Top -> "⊤"
  Atom (Prop fol) -> fol
  Not for -> "¬" ++ printProp for
  And for for' -> "(" ++ printProp for ++ "∧" ++ printProp for' ++ ")"
  Or for for' -> "(" ++ printProp for ++ "∨" ++ printProp for' ++ ")"
  Imp for for' -> "(" ++ printProp for ++ "→" ++ printProp for' ++ ")"
  Iff for for' -> "(" ++ printProp for ++ "⟷" ++ printProp for' ++ ")"
  Forall s for -> "∀" ++ s ++ printProp for
  Exists s for -> "∃" ++ s ++ printProp for

printprops :: [Formula Prop] -> String
printprops ps = intercalate ", " (map printProp ps)

printFormulaASCII :: Formula Fol -> String 
printFormulaASCII f = case f of  
  Bot -> "⊥"
  Top -> "⊤"
  Atom fol -> printFol fol
  Not for -> "~" ++ printFormulaASCII for
  And for for' -> "(" ++ printFormulaASCII for ++ "&" ++ printFormulaASCII for' ++ ")"
  Or for for' -> "(" ++ printFormulaASCII for ++ "v" ++ printFormulaASCII for' ++ ")"
  Imp for for' -> "(" ++ printFormulaASCII for ++ "->" ++ printFormulaASCII for' ++ ")"
  Iff for for' -> "(" ++ printFormulaASCII for ++ "<->" ++ printFormulaASCII for' ++ ")"
  Forall s for -> "@" ++ s ++ printFormulaASCII for
  Exists s for -> "#" ++ s ++ printFormulaASCII for

printPropASCII :: Formula Prop -> String 
printPropASCII f = case f of  
  Bot -> "⊥"
  Top -> "⊤"
  Atom (Prop fol) -> fol
  Not for -> "~" ++ printPropASCII for
  And for for' -> "(" ++ printPropASCII for ++ "&" ++ printPropASCII for' ++ ")"
  Or for for' -> "(" ++ printPropASCII for ++ "v" ++ printPropASCII for' ++ ")"
  Imp for for' -> "(" ++ printPropASCII for ++ "->" ++ printPropASCII for' ++ ")"
  Iff for for' -> "(" ++ printPropASCII for ++ "<->" ++ printPropASCII for' ++ ")"
  Forall s for -> "@" ++ s ++ printPropASCII for
  Exists s for -> "#" ++ s ++ printPropASCII for

printpropsASCII :: [Formula Prop] -> String
printpropsASCII ps = intercalate ", " (map printPropASCII ps)