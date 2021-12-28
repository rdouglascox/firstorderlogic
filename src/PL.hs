module PL where

import Data.Prop

import Data.List ( nub ) 

-- | apply a function to all atoms in a formula

onatoms :: (a -> Formula a) -> Formula a -> Formula a
onatoms f fm = 
    case fm of 
        Atom a -> f a
        Not p -> Not (onatoms f p) 
        And p q -> And (onatoms f p) (onatoms f q)
        Or p q -> Or (onatoms f p) (onatoms f q)
        Imp p q -> Imp (onatoms f p) (onatoms f q)
        Iff p q -> And (onatoms f p) (onatoms f q)
        Forall x p -> Forall x (onatoms f p) 
        Exists x p -> Exists x (onatoms f p)
        _ -> fm

-- | iterate a binary function over all atoms of a formula

overatoms :: (t1 -> t2 -> t2) -> Formula t1 -> t2 -> t2
overatoms f fm b =
    case fm of
        Atom a -> f a b
        Not p -> overatoms f p b
        And p q -> overatoms f p (overatoms f q b)
        Or p q -> overatoms f p (overatoms f q b)
        Imp p q -> overatoms f p (overatoms f q b)
        Iff p q -> overatoms f p (overatoms f q b)
        Forall x p -> overatoms f p b
        Exists x p -> overatoms f p b
        _ -> b

atomunion :: Eq a => (t -> [a]) -> Formula t -> [a]
atomunion f fm = nub (overatoms (\h t -> f h ++ t) fm [])

-- | propositional simplication
simplify :: Formula Fol -> Formula Fol
simplify fm = case fm of
  Not p -> psimplify1 (Not (simplify p))
  And p q -> psimplify1 (And (simplify p) (simplify q))
  Or p q -> psimplify1 (Or (simplify p) (simplify q))
  Imp p q -> psimplify1 (Imp (simplify p) (simplify q))
  Iff p q -> psimplify1 (Iff (simplify p) (simplify q))
  _ -> fm

psimplify1 :: Formula Fol -> Formula Fol
psimplify1 fm = case fm of
  Not Bot -> Top
  Not Top -> Bot
  Not (Not p) -> p
  And p Bot -> Bot
  And Bot p -> Bot
  And p Top -> p
  And Top p -> p
  Or p Bot -> p
  Or Bot p -> p
  Or p Top -> Top
  Or Top p -> Top
  Imp Bot p -> Top
  Imp p Top -> Top
  Imp Top p -> p
  Imp p Bot -> Not p
  Iff p Top -> p
  Iff Top p -> p
  Iff p Bot -> Not p
  Iff Bot p -> Not p
  _ -> fm


