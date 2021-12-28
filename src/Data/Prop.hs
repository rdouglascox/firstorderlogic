module Data.Prop where

data Term = Var String 
          | Fn String [Term]
          deriving (Show,Eq,Ord)

data Fol = R String [Term]
        deriving (Show,Eq,Ord)

newtype Prop = Prop String

data Formula a = Bot 
               | Top
               | Atom a
               | Not (Formula a)
               | And (Formula a) (Formula a)
               | Or (Formula a) (Formula a)
               | Imp (Formula a) (Formula a)
               | Iff (Formula a) (Formula a)
               | Forall String (Formula a)
               | Exists String (Formula a)
               deriving (Show,Eq,Ord)

-- e.g. we will then have function involving "Formula Fol"