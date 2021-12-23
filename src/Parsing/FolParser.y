{
module Parsing.FolParser where
import Parsing.FolToken
import Data.Prop
}

%name happyFolParser
%tokentype { FolToken }
%error { parseError }
%monad { E } { thenE } { returnE }

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

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      Ok a -> Ok a
      Failed e -> k e

parseError tokens = failE "Parse error"


}