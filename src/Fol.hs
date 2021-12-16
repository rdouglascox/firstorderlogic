module Fol where

import Data.Prop
import Data.List
import qualified Data.Map as Map

termval m@(domain,func,pred) v tm =
    case tm of
        Var x -> v x
        Fn f args -> func f (map (termval m v) args)

holds m@(domain,func,pred) v fm =
    case fm of
        bot -> False
        top -> True
        Atom (R r args) -> pred r (map (termval m v) args)
        Not p -> not (holds m v p)
        And p q -> holds m v p && holds m v p
        Or p q -> holds m v p || holds m v p
        Imp p q -> not (holds m v p) || holds m v p
        Iff p q -> holds m v p == holds m v p
        Forall x p -> all (\a -> holds m (Map.insert x a v) p) domain
        Exists x p -> any (\a -> holds m (Map.insert x a v) p) domain

fvt tm =
    case tm of
        
