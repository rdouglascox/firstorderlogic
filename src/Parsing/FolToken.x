{
module Parsing.FolToken where
}

%wrapper "basic"

$upper = A-Z
$lower1a = a-e
$lower1b = j-t
$lower2a = x-z
$lower2b = w
$lower2c = u
$lower3 = f-h

tokens :-

  $white+                             ;
  $upper+                             { \s -> PredicateSymbol s }
  $lower1a+                           { \s -> ConstantSymbol s }
  $lower1b+                           { \s -> ConstantSymbol s }
  $lower2a+                            { \s -> VariableSymbol s }
  $lower2b+                            { \s -> VariableSymbol s }
  $lower2c+                            { \s -> VariableSymbol s }
  $lower3+                            { \s -> FunctionSymbol s }
  "True"                              { \s -> TopSymbol}
  "False"                             { \s -> BotSymbol}
  "~"                                 { \s -> NegationSymbol }
  "&"                                 { \s -> ConjunctionSymbol }
  "v"                                 { \s -> DisjunctionSymbol }
  "->"                                { \s -> ConditionalSymbol }
  "<->"                               { \s -> BiconditionalSymbol }
  "@"                                 { \s -> UniversalSymbol }
  "#"                                 { \s -> ExistentialSymbol }
  "("                                 { \s -> LeftPar }
  ")"                                 { \s -> RightPar }
  
{

data FolToken = PredicateSymbol String
               | VariableSymbol String
               | ConstantSymbol String
               | FunctionSymbol String
               | TopSymbol
               | BotSymbol
               | NegationSymbol
               | ConjunctionSymbol
               | DisjunctionSymbol
               | ConditionalSymbol
               | BiconditionalSymbol
               | UniversalSymbol
               | ExistentialSymbol
               | LeftPar
               | RightPar
               deriving (Eq,Show)

} 