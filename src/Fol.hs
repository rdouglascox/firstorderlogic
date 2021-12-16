module Fol where

import Data.Prop
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

termval :: (a, String -> [b] -> b, c) -> Map.Map String b -> Term -> b
termval m@(domain,func,pred) v tm =
    case tm of
        Var x -> case Map.lookup x v of
            Nothing -> error "not in domain"
            Just y -> y
        Fn f args -> func f (map (termval m v) args)

holds :: Foldable t => (t a, String -> [a] -> a, String -> [a] -> Bool) -> Map.Map String a -> Formula Fol -> Bool
holds m@(domain,func,pred) v fm =
    case fm of
        Bot -> False
        Top -> True
        Atom (R r args) -> pred r (map (termval m v) args)
        Not p -> not (holds m v p)
        And p q -> holds m v p && holds m v p
        Or p q -> holds m v p || holds m v p
        Imp p q -> not (holds m v p) || holds m v p
        Iff p q -> holds m v p == holds m v p
        Forall x p -> all (\z -> holds m (Map.insert x z v) p) domain
        Exists x p -> any (\z -> holds m (Map.insert x z v) p) domain

-- | FVT(t), the set of all the variables involved in a term t
fvt :: Term -> [String]
fvt tm =
    case tm of
        Var x -> [x]
        Fn f args -> myunions (map fvt args)

myunions :: Ord a => [[a]] -> [a]
myunions xs = Set.toList $ Set.unions $ Set.map Set.fromList (Set.fromList xs)

-- | all the variables occurring in a formula
var :: Formula Fol -> [String]
var fm =
    case fm of
        Top -> []
        Bot -> []
        Atom (R p args) -> myunions (map fvt args)
        Not p -> var p
        And p q -> var p `union` var q
        Or p q -> var p `union` var q
        Imp p q -> var p `union` var q
        Iff p q -> var p `union` var q
        Forall x p -> insert x (var p)
        Exists x p -> insert x (var p)

-- | all the free variables occurring in a formula
fv :: Formula Fol -> [String]
fv fm =
    case fm of
        Top -> []
        Bot -> []
        Atom (R p args) -> myunions (map fvt args)
        Not p -> fv p
        And p q -> fv p `union` fv q
        Or p q -> fv p `union` fv q
        Imp p q -> fv p `union` fv q
        Iff p q -> fv p `union` fv q
        Forall x p ->  fv p \\ [x]
        Exists x p ->  fv p \\ [x]

-- | universally quantify over all free variable in a formula
generalize :: Formula Fol -> Formula Fol
generalize fm = foldr Forall fm( fv  fm)

-- | substitution in terms
tsubst :: Map.Map String Term -> Term -> Term
tsubst sfn tm =
    case tm of
      Var x -> Data.Maybe.fromMaybe tm (Map.lookup x sfn)
      Fn f args -> Fn f (map (tsubst sfn) args)



-- | substitution in formulas

variant :: Foldable t => String -> t String -> String
variant x vars = if x `elem` vars
     then variant (x ++ "'") vars
     else x


subst subfn fm =
    case fm of
        Bot -> Bot
        Top -> Top
        Atom (R p args) -> Atom (R p (map (tsubst subfn) args))
        Not p -> Not (subst subfn p)
        And p q -> And (subst subfn p) (subst subfn q)
        Or p q -> Or (subst subfn p) (subst subfn q)
        Imp p q -> Imp (subst subfn p) (subst subfn q)
        Iff p q -> Iff (subst subfn p) (subst subfn q)
        Forall x p -> substq subfn Forall x p
        Exists x p -> substq subfn Exists x p

substq subfn quant x p =
    let x' = if any (\k -> case Map.lookup k subfn of
                                 Just v -> x `elem` fv v
                                 Nothing -> False) (fv p \\ [x])
                then variant x (fv (subst (Map.delete x subfn) p))
                else x in
                    quant x' (subst (Map.insert x (Var x') subfn) p)

