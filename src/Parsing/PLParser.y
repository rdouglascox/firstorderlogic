{
module Parsing.PLParser (happyParser) where
import Parsing.PLToken1
import Data.PLprop (Prop (..))
}

%name happyParser
%tokentype { PLToken }
%error { parseError }
%monad {Maybe}

%token
    basic        { BasicSymbol $$ }
    "~"          { NegationSymbol }
    "&"          { ConjunctionSymbol }
    "v"          { DisjunctionSymbol }
    "->"         { ConditionalSymbol }
    "<->"        { BiconditionalSymbol }
    "("          { LeftPar }
    ")"          { RightPar }

%%

Prop : basic         { Basic $1 }
     | "~" Prop                  { Negation $2 } 
     | "(" Prop "&" Prop ")"     { Conjunction $2 $4 }  
     | "(" Prop "v" Prop ")"     { Disjunction $2 $4 }
     | "(" Prop "->" Prop ")"    { Conditional $2 $4 }
     | "(" Prop "<->" Prop ")"   { Biconditional $2 $4 }

{

parseError :: [PLToken] -> Maybe a
parseError _ = Nothing

}

