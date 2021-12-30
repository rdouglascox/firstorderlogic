module PL where

import Data.Prop
import qualified Data.PLprop as PL

import Data.List ( nub, union, partition, intersect, (\\), find, minimum, maximum, lookup, intercalate)

import Prelude hiding (negate)
import Data.Maybe

import Parsing.BNFPLParser (parse)  -- for testing
import Control.Monad.Writer

import qualified Data.Set as S

import Printing.Printer

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
psimplify :: Formula a -> Formula a
psimplify fm = case fm of
  Not p -> psimplify1 (Not (psimplify p))
  And p q -> psimplify1 (And (psimplify p) (psimplify q))
  Or p q -> psimplify1 (Or (psimplify p) (psimplify q))
  Imp p q -> psimplify1 (Imp (psimplify p) (psimplify q))
  Iff p q -> psimplify1 (Iff (psimplify p) (psimplify q))
  _ -> fm

psimplify1 :: Formula a -> Formula a
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
  Iff Bot Bot -> Top
  Iff p Bot -> Not p
  Iff Bot p -> Not p
  _ -> fm

-- is the formula a negative literal?
negative :: Formula a -> Bool
negative (Not p) = True
negative _ = False

-- is the formula a positive literal? 
positive :: Formula a -> Bool
positive = not . negative

-- negate a formula 
negate :: Formula a -> Formula a
negate (Not p) = p
negate x = Not x

-- put a formula in negation normal form

nnf :: Formula a -> Formula a
nnf fm = case fm of
    And p q -> And (nnf p) (nnf q)
    Or p q -> Or (nnf p) (nnf q)
    Imp p q -> Or (nnf (Not p)) (nnf q)
    Iff p q -> Or (And (nnf p) (nnf q)) (And (nnf (Not p)) (nnf (Not q)))
    Not (Not p) -> nnf p
    Not (And p q) -> Or (nnf (Not p)) (nnf (Not q))
    Not (Or p q) -> And (nnf (Not p)) (nnf (Not q))
    Not (Imp p q) -> And (nnf p) (nnf (Not q))
    Not (Iff p q) -> Or (And (nnf p) (nnf (Not q))) (And (nnf (Not p)) (nnf q))
    Forall x p -> Forall x (nnf p)
    Exists x p -> Exists x (nnf p)
    Not (Forall x p) -> Exists x (nnf (Not p))
    Not (Exists x p) -> Forall x (nnf (Not p))
    _ -> fm

-- | simplify and apply
nnf' :: Formula a -> Formula a
nnf' = nnf . psimplify

-- just push negation down, but avoid explosion from biconditionals
nenf :: Formula a -> Formula a
nenf fm =
    case fm of
    Not (Not p) -> nenf p
    Not (And p q) -> Or (nenf (Not p)) (nenf (Not q))
    Not (Or p q) -> And (nenf (Not p)) (nenf (Not q))
    Not (Imp p q) -> And (nenf p) (nenf (Not q))
    Not (Iff p q) -> Iff (nenf p) (nenf (Not q))
    And p q -> And (nenf p) (nenf q)
    Or p q -> Or (nenf p) (nenf q)
    Imp p q -> Or (nenf (Not p)) (nenf q)
    Iff p q -> Iff (nenf p) (nenf q)
    _ -> fm

-- simplify and apply
nenf' :: Formula a -> Formula a
nenf' = nenf . psimplify

-- DISJUNCTIVE NORMAL FORM

-- apply the distributive laws
distrib' :: Formula a -> Formula a
distrib' (And p (Or q r)) = Or (distrib' (And p q)) (distrib' (And p r))
distrib' (And (Or p q) r) = Or (distrib' (And p r)) (distrib' (And q r))
distrib' x = x

-- raw disjunctive normal form
rawdnf :: Formula a -> Formula a
rawdnf (And p q) = distrib' (And (rawdnf p) (rawdnf q))
rawdnf (Or p q) = Or (rawdnf p) (rawdnf q)
rawdnf x = x

-- set based representations 
-- set-based versions of top level functions have an 'S' at the end.

type ClauseSet a = S.Set (S.Set (Formula a))

distribS :: Ord a => ClauseSet a -> ClauseSet a -> ClauseSet a
distribS xs ys = S.fromList $ [S.union x y | x <- S.toList xs, y <- S.toList ys]

distrib :: Ord a => [[Formula a]] -> [[Formula a]] -> [[Formula a]]
distrib xs ys = S.toList (S.map S.toList (distribS (S.fromList (map S.fromList xs))(S.fromList (map S.fromList ys))))

purednf :: Ord a => Formula a -> ClauseSet a
purednf fm =
    case fm of
        And p q -> distribS (purednf p) (purednf q)
        Or p q -> purednf p `S.union` purednf q
        _ -> S.singleton (S.singleton fm)

trivial :: Ord a => S.Set (Formula a) -> Bool
trivial lits = let (pos,neg) = S.partition positive lits in
   S.intersection pos (S.map negate neg) /= S.empty

simpdnfS :: Ord a => Formula a -> ClauseSet a
simpdnfS fm = let djs = S.filter (not . trivial) (purednf (nnf fm)) in
    S.filter (\d -> not (any (`S.isProperSubsetOf` d) djs)) djs

simpdnf :: Ord a => Formula a -> [[Formula a]]
simpdnf f = S.toList (S.map S.toList (simpdnfS f))

listdisjS :: S.Set (Formula a) -> Formula a
listdisjS s = if S.null s then Bot
    else foldr1 Or s

listconjS :: S.Set (Formula a) -> Formula a
listconjS s = if S.null s then Top
    else foldr1 And s

listdisj :: [Formula a] -> Formula a
listdisj s = if null s then Bot
    else foldr1 Or s

listconj :: [Formula a] -> Formula a
listconj s = if null s then Top
    else foldr1 And s

dnf :: Ord a => Formula a -> Formula a
dnf fm = listdisjS (S.map listconjS (simpdnfS fm))

dnfsat :: Ord a => [Formula a] -> Bool
dnfsat p = not $ S.null $ simpdnfS (foldr1 And p)

-- CONJUNCTIVE NORMAL FORM

purecnf :: Ord a => Formula a -> ClauseSet a
purecnf fm = S.map (S.map negate) (purednf (nnf (Not fm)))

simpcnfS :: Ord a => Formula a -> ClauseSet a
simpcnfS fm = let cjs = S.filter (not. trivial) (purecnf fm) in
    S.filter (\x -> not (any (`S.isProperSubsetOf` x) cjs)) cjs

cnf :: Ord a => Formula a -> Formula a
cnf fm = listconjS (S.map listdisjS (simpcnfS fm))

-- RAMSEY proposition generator 

ramsey s t n =
    let vertices = [1 .. n] in
    let yesgrps = map (allSets 2) (allSets s vertices) in
    let nogrps = map (allSets 2) (allSets t vertices) in
    let e [m,n'] = Atom $ Prop ("p_" ++ show m ++ "_" ++ show n') in
    Or (listdisj $ map (listconj . map e ) yesgrps) (listdisj $ map (listconj . map (Not . e)) nogrps)

allSets :: Int -> [a] -> [[a]]
allSets 0 _ = [[]]
allSets _ [] = []
allSets m (h:t) = map (h:) (allSets (m - 1) t) ++ allSets m t


-- THE DAVIS PUTNAM PROCEDURE

-- | first the "one literal rule" (this rule is correct)
oneliteralrule :: Ord a => ClauseSet a -> Maybe (ClauseSet a)
oneliteralrule clauses =
    let mu = setFind unarySet clauses in case mu of
      Nothing -> Nothing
      Just set -> let u = head $ S.toList set in
                  let u' = negate u in
          let clauses1 = S.filter (not . S.member u) clauses in
        --      Just $ S.map (\x -> S.difference x (S.singleton u')) clauses1
              Just $ S.map (S.\\ S.singleton u') clauses1

unarySet :: S.Set (Formula a) -> Bool
unarySet s = setSize s == 1

setFind ::(a -> Bool) -> S.Set a -> Maybe a
setFind p xs = find p $ S.toList xs

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

setSize :: S.Set a-> Int
setSize = S.foldr (\_ n -> 1 + n) 0

-- |second, the affirmative-negative rule (or the pure literal rule)

affirmativenegativerule :: Ord a => ClauseSet a -> Maybe (ClauseSet a)
affirmativenegativerule clauses =
    let (neg',pos) = S.partition negative (S.unions clauses) in
    let neg = S.map negate neg' in
    let posonly = pos S.\\ neg in
    let negonly = neg S.\\ pos in
    let pure = S.union posonly (S.map negate negonly) in
    if pure == S.empty
        then Nothing
        else Just $ S.filter (\x -> S.intersection x pure == S.empty) clauses

-- | third the resolution rule

resolveon :: Ord a => Formula a -> ClauseSet a -> Maybe (ClauseSet a)
resolveon p clauses =
    let p' = negate p in
    let (pos,notpos) = S.partition (S.member p) clauses in
    let (neg,other) = S.partition (S.member p') notpos in
    let pos' = S.map (S.filter (/= p)) pos in
    let neg' = S.map (S.filter (/= p')) neg in
    let res0 = S.fromList $ [S.union x y | x <- S.toList pos', y <- S.toList neg'] in
    Just $ S.union other (S.filter (not . trivial) res0)

-- | resolution blowup

resolutionblowup :: Ord a => ClauseSet a -> Formula a -> Int
resolutionblowup cls l =
    let m = setSize (S.filter (S.member l) cls) in
    let n = setSize (S.filter (S.member (negate l)) cls) in
    m * n - m - n

-- | resolution rule

resolutionrule :: Ord a => ClauseSet a -> Maybe (ClauseSet a)
resolutionrule clauses =
    let pvs = S.filter positive (S.unions clauses) in
    let p = minimize (resolutionblowup clauses) pvs in
    resolveon p clauses

minimize :: Ord a => (a -> Int) -> S.Set a -> a
minimize f xs = let ordindex = map f (S.toList xs) in
    fromJust $ lookup (minimum ordindex) (zip ordindex (S.toList xs))

maximize :: Ord a => (a -> Int) -> S.Set a -> a
maximize f xs = let ordindex = map f (S.toList xs) in
    fromJust $ lookup (maximum ordindex) (zip ordindex (S.toList xs))

-- the dp procedure

dp :: Ord a => ClauseSet a -> Bool
dp clauses
  | clauses == S.empty = True
  | S.member S.empty clauses = False
  | otherwise = case oneliteralrule clauses of
      Nothing -> case affirmativenegativerule clauses of
          Nothing -> case resolutionrule clauses of
              Nothing -> error "This shouldn't happen"
              Just set -> dp set
          Just set -> dp set
      Just set -> dp set

-- | dp sat

dpsats :: Ord a => [Formula a] -> Bool
dpsats ps = dp (simpcnfS (foldr1 And ps))

dpsat :: Ord a => Formula a -> Bool
dpsat fm = dp (simpcnfS fm)

dptaut :: Ord a => Formula a -> Bool
dptaut fm = not (dpsat(Not fm))


posnegcount :: Ord a => ClauseSet a -> Formula a -> Int
posnegcount cls l =
    let m = setSize (S.filter (S.member l) cls) in
    let n = setSize (S.filter (S.member (negate l)) cls) in
    m + n

dpll :: Ord a => ClauseSet a -> Bool
dpll clauses
  | clauses == S.empty = True
  | S.member S.empty clauses = False
  | otherwise = case oneliteralrule clauses of
      Nothing -> case affirmativenegativerule clauses of
          Nothing -> let pvs = S.filter positive (S.unions clauses) in
                     let p = maximize (posnegcount clauses) pvs in
                     dpll (S.insert (S.singleton p) clauses) || dpll (S.insert (S.singleton (negate p)) clauses)
          Just set -> dpll set
      Just set -> dpll set

-- | dp sat

dpllsats :: Ord a => [Formula a] -> Bool
dpllsats ps = dpll (simpcnfS (foldr1 And ps))

dpllsat :: Ord a => Formula a -> Bool
dpllsat fm = dpll (simpcnfS fm)

dplltaut :: Ord a => Formula a -> Bool
dplltaut fm = not (dpllsat(Not fm))

-- legacy compatibility code! 

convert :: PL.Prop -> Formula Prop
convert p = case p of
  PL.Basic s -> Atom (Prop s)
  PL.Negation pr -> Not (convert pr)
  PL.Conjunction pr pr' -> And (convert pr) (convert pr')
  PL.Disjunction pr pr' -> Or (convert pr) (convert pr')
  PL.Conditional pr pr' -> Imp (convert pr) (convert pr')
  PL.Biconditional pr pr' -> Iff (convert pr) (convert pr')

dpsatleg :: [PL.Prop] -> Bool
dpsatleg xs = dpsats (map convert xs)

--}


