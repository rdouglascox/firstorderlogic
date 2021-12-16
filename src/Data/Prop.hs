module Data.Prop where

data Term = Var String 
          | Fn String [Term]

data Fol = R String [Term]

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

-- e.g. we will then have function involving "Formula Fol"