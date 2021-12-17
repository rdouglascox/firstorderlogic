{
module Parsing.FolParser (happyFolParser) where
import Parsing.FolToken
import Data.Prop
}

%name happyFolParser
%tokentype { FolToken }
%error { parseError }

%token
    pred         { PredicateSymbol $$ }
    const        { ConstantSymbol $$ }
    vari         { VariableSymbol $$ }
    "~"          { NegationSymbol }
    "&"          { ConjunctionSymbol }
    "v"          { DisjunctionSymbol }
    "->"         { ConditionalSymbol }
    "<->"        { BiconditionalSymbol }
    "@"          { UniversalSymbol }
    "#"          { ExistentialSymbol }
    "("          { LeftPar }
    ")"          { RightPar }

%%

prop : pred terms                { Atomic (Predicate $1) $2  }
     | "~" prop                  { Negation $2 } 
     | "(" prop "&" prop ")"     { Conjunction $2 $4 }  
     | "(" prop "v" prop ")"     { Disjunction $2 $4 }
     | "(" prop "->" prop ")"    { Conditional $2 $4 }
     | "(" prop "<->" prop ")"   { Biconditional $2 $4 }
     | "@" vari prop             { Universal $2 $3 }
     | "#" vari prop             { Existential $2 $3 }

terms : const                { [Constant $1] }
      | vari                 { [Variable $1] }      
      | const terms          { Constant $1 : $2 }
      | vari terms           { Variable $1 : $2 }


{

parseError :: [GPLIToken] -> a
parseError _ = error "Parse Error"

}