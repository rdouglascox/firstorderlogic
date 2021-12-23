{
module Parsing.FolParser (happyFolParser) where
import Parsing.FolToken
import Data.Prop
}

%name happyFolParser
%tokentype { FolToken }
%error { parseError }
%monad {Maybe}

%token
    pred         { PredicateSymbol $$ }
    const        { ConstantSymbol $$ }
    vari         { VariableSymbol $$ }
    funct        { FunctionSymbol $$ }
    top          { TopSymbol }
    bot          { BotSymbol }
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

form : fol                       { Atom $1 }
     | bot                       { Bot }
     | top                       { Top }
     | "~" form                  { Not $2 } 
     | "~" "(" form ")"          { Not $3 }
     | "(" form "&" form ")"     { And $2 $4 }  
     | "(" form "v" form ")"     { Or $2 $4 }
     | "(" form "->" form ")"    { Imp $2 $4 }
     | "(" form "<->" form ")"   { Iff $2 $4 }
     | "@" vari form             { Forall $2 $3 }
     | "#" vari form             { Exists $2 $3 }

fol : pred terms                 {R $1 $2 }
    | pred                       {R $1 [] }

terms : const                { [Fn $1 []] }
      | vari                 { [Var $1] } 
      | funct "(" terms ")"  { [Fn $1 $3] }
      | const terms          { (Fn $1 []) : $2 }
      | vari terms           { (Var $1) : $2 } 
      | funct "(" terms ")" terms { (Fn $1 $3) : $5 }
  


{


parseError _ = error "Parse Error"


}