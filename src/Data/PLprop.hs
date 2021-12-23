module Data.PLprop (Prop (..)) where

data Prop = Basic String
          | Negation Prop
          | Conjunction Prop Prop
          | Disjunction Prop Prop
          | Conditional Prop Prop
          | Biconditional Prop Prop
    deriving (Show,Eq,Ord)