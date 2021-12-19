module DPLL where

import Data.Prop

import Control.Applicative ( Applicative(liftA2) )

import Printing.Printer

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Maybe ( fromJust )

nnf :: Formula a -> Formula a
nnf (And p q) = And (nnf p) (nnf q)
nnf (Or p q) = Or (nnf p) (nnf q)
nnf (Imp p q) = Or (nnf (Not p)) (nnf q)
nnf (Iff p q) = Or (And (nnf p) (nnf q)) (And (nnf (Not p))(nnf (Not q)))
nnf (Not (Not p)) = nnf p
nnf (Not (And p q)) = Or (nnf (Not p)) (nnf (Not q))
nnf (Not (Or p q)) = And (nnf (Not p)) (nnf (Not q))
nnf (Not (Imp p q)) = And (nnf p) (nnf (Not q))
nnf (Not (Iff p q)) = Or (And (nnf p) (nnf (Not q))) (And (nnf (Not p)) (nnf q))
nnf x = x

dnf' :: Formula a -> Formula a
dnf' = rawdnf . nnf

distrib :: Formula a -> Formula a
distrib (And p (Or q r)) = Or (distrib (And p q)) (distrib (And p r))
distrib (And (Or p q) r) = Or (distrib (And p r)) (distrib (And q r))
distrib x = x

rawdnf :: Formula a -> Formula a
rawdnf (And p q) = distrib (And (rawdnf p) (rawdnf q))
rawdnf (Or p q) = Or (rawdnf p) (rawdnf q)
rawdnf x = x

distrib' :: Ord a => S.Set (S.Set (Formula a)) -> S.Set (S.Set (Formula a)) -> S.Set (S.Set (Formula a))
distrib' xs ys = S.fromList $ [S.union x y | x <- S.toList xs, y <- S.toList ys]

purednf :: Ord a => Formula a -> S.Set (S.Set (Formula a))
purednf (And p q) = distrib' (purednf p) (purednf q)
purednf (Or p q) = S.union (purednf p) (purednf q)
purednf x = S.singleton (S.singleton x)

negative :: Formula a -> Bool
negative (Not p) = True
negative _ = False

positive :: Formula a -> Bool
positive = not . negative

negate' :: Formula a -> Formula a
negate' (Not p) = p
negate' x = Not x

trivial :: Ord a => S.Set (Formula a) -> Bool
trivial lits = let (pos,neg) = S.partition positive lits in
   S.intersection pos (S.map negate' neg) /= S.empty

simpdnf :: Ord a => Formula a -> S.Set (S.Set (Formula a))
simpdnf fm = let djs = S.filter (not . trivial) (purednf (nnf fm)) in
    S.filter (\d -> not (any (`S.isProperSubsetOf` d) djs)) djs

-- | sat checking based on simpdnf (set based dnf representation)
dnfsat :: Ord a => [Formula a] -> Bool
dnfsat p = not $ S.null $ simpdnf (foldr1 And p)

listdisj :: S.Set (Formula a) -> Formula a
listdisj s = if S.null s then Top
    else foldr1 Or (S.elems s)

listconj :: S.Set (Formula a) -> Formula a
listconj s = if S.null s then Bot
    else foldr1 And (S.elems s)

dnf :: Ord a => Formula a -> Formula a
dnf fm = listdisj (S.map listconj (simpdnf fm))

purecnf :: Ord a => Formula a -> S.Set (S.Set (Formula a))
purecnf fm = S.map (S.map negate') (purednf (nnf (Not fm)))

simpcnf :: Ord a => Formula a -> S.Set (S.Set (Formula a))
simpcnf fm = let cjs = S.filter (not. trivial) (purecnf fm) in
    S.filter (\x -> not (any (`S.isProperSubsetOf` x) cjs)) cjs

cnf :: Ord a => Formula a -> Formula a
cnf fm = listconj (S.map listdisj (simpcnf fm))

-- | okay, here we go, davis-putnam

-- | first the "one literal rule" (this rule is correct)
oneliteralrule :: Ord a => S.Set (S.Set (Formula a)) -> Maybe (S.Set (S.Set (Formula a)))
oneliteralrule clauses =
    let mu = setFind unarySet clauses in case mu of
      Nothing -> Nothing
      Just set -> let u = head $ S.toList set in
                  let u' = negate' u in
          let clauses1 = S.filter (not . S.member u) clauses in
        --      Just $ S.map (\x -> S.difference x (S.singleton u')) clauses1
              Just $ S.map (S.\\ S.singleton u') clauses1

unarySet :: S.Set (Formula a) -> Bool
unarySet s = setSize s == 1

setFind ::(a -> Bool) -> S.Set a -> Maybe a
setFind p xs = L.find p $ S.toList xs

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

setSize :: S.Set a-> Int
setSize = S.foldr (\_ n -> 1 + n) 0

-- |second, the affirmative-negative rule (or the pure literal rule)

affirmativenegativerule :: Ord a => S.Set (S.Set (Formula a)) -> Maybe (S.Set (S.Set (Formula a)))
affirmativenegativerule clauses =
    let (neg',pos) = S.partition negative (S.unions clauses) in
    let neg = S.map negate' neg' in
    let posonly = pos S.\\ neg in
    let negonly = neg S.\\ pos in
    let pure = S.union posonly (S.map negate' negonly) in
    if pure == S.empty
        then Nothing
        else Just $ S.filter (\x -> S.intersection x pure == S.empty) clauses

-- | third the resolution rule

resolveon :: Ord a => Formula a -> S.Set (S.Set (Formula a)) -> Maybe (S.Set (S.Set (Formula a)))
resolveon p clauses =
    let p' = negate' p in
    let (pos,notpos) = S.partition (S.member p) clauses in
    let (neg,other) = S.partition (S.member p') notpos in
    let pos' = S.map (S.filter (/= p)) pos in
    let neg' = S.map (S.filter (/= p')) neg in
    let res0 = S.fromList $ [S.union x y | x <- S.toList pos', y <- S.toList neg'] in
    Just $ S.union other (S.filter (not . trivial) res0)

-- | resolution blowup

resolutionblowup :: Ord a => S.Set (S.Set (Formula a)) -> Formula a -> Int
resolutionblowup cls l =
    let m = setSize (S.filter (S.member l) cls) in
    let n = setSize (S.filter (S.member (negate' l)) cls) in
    m * n - m - n

-- | looking for the literal that occurs most

posnegcount :: Ord a => S.Set (S.Set (Formula a)) -> Formula a -> Int
posnegcount cls l =
    let m = setSize (S.filter (S.member l) cls) in
    let n = setSize (S.filter (S.member (negate' l)) cls) in
    m + n

-- | resolution rule

resolutionrule :: Ord a => S.Set (S.Set (Formula a)) -> Maybe (S.Set (S.Set (Formula a)))
resolutionrule clauses =
    let pvs = S.filter positive (S.unions clauses) in
    let p = minimize (resolutionblowup clauses) pvs in
    resolveon p clauses

minimize :: (a -> Int) -> S.Set a -> a
minimize f xs = let ordindex = map f (S.toList xs) in
    fromJust $ L.lookup (minimum ordindex) (zip ordindex (S.toList xs))

maximize :: (a -> Int) -> S.Set a -> a
maximize f xs = let ordindex = map f (S.toList xs) in
    fromJust $ L.lookup (maximum ordindex) (zip ordindex (S.toList xs))

-- the dp procedure

dpll :: Ord a => S.Set (S.Set (Formula a)) -> Bool
dpll clauses
  | clauses == S.empty = True
  | S.member S.empty clauses = False
  | otherwise = case oneliteralrule clauses of
      Nothing -> case affirmativenegativerule clauses of
          Nothing -> let pvs = S.filter positive (S.unions clauses) in
                     let p = maximize (posnegcount clauses) pvs in
                     dpll (S.insert (S.singleton p) clauses) || dpll (S.insert (S.singleton (negate' p)) clauses)
          Just set -> dpll set
      Just set -> dpll set

-- | dp sat

dpllsat :: Ord a => [Formula a] -> Bool
dpllsat ps = dpll (simpcnf (foldr1 And ps))

