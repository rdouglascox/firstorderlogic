module DP (dnfsat,dpsat,cnf,dnf,nnf,dnf',simpcnf,dptrace,printSets,dodptrace) where

import Data.PLprop

import Control.Applicative ( Applicative(liftA2) )

import Printing.PLprop

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Maybe ( fromJust )

import Text.Blaze.Html as H 
import qualified Text.Blaze.Html5 as H5

import Control.Monad.Writer

nnf :: Prop -> Prop
nnf (Conjunction p q) = Conjunction (nnf p) (nnf q)
nnf (Disjunction p q) = Disjunction (nnf p) (nnf q)
nnf (Conditional p q) = Disjunction (nnf (Negation p)) (nnf q)
nnf (Biconditional p q) = Disjunction (Conjunction (nnf p) (nnf q)) (Conjunction (nnf (Negation p))(nnf (Negation q)))
nnf (Negation (Negation p)) = nnf p
nnf (Negation (Conjunction p q)) = Disjunction (nnf (Negation p)) (nnf (Negation q))
nnf (Negation (Disjunction p q)) = Conjunction (nnf (Negation p)) (nnf (Negation q))
nnf (Negation (Conditional p q)) = Conjunction (nnf p) (nnf (Negation q))
nnf (Negation (Biconditional p q)) = Disjunction (Conjunction (nnf p) (nnf (Negation q))) (Conjunction (nnf (Negation p)) (nnf q))
nnf x = x

dnf' :: Prop -> Prop
dnf' = rawdnf . nnf

distrib :: Prop -> Prop
distrib (Conjunction p (Disjunction q r)) = Disjunction (distrib (Conjunction p q)) (distrib (Conjunction p r))
distrib (Conjunction (Disjunction p q) r) = Disjunction (distrib (Conjunction p r)) (distrib (Conjunction q r))
distrib x = x

rawdnf :: Prop -> Prop
rawdnf (Conjunction p q) = distrib (Conjunction (rawdnf p) (rawdnf q))
rawdnf (Disjunction p q) = Disjunction (rawdnf p) (rawdnf q)
rawdnf x = x

distrib' :: S.Set (S.Set Prop) -> S.Set (S.Set Prop) -> S.Set (S.Set Prop)
distrib' xs ys = S.fromList $ [S.union x y | x <- S.toList xs, y <- S.toList ys]

purednf :: Prop -> S.Set (S.Set Prop)
purednf (Conjunction p q) = distrib' (purednf p) (purednf q)
purednf (Disjunction p q) = S.union (purednf p) (purednf q)
purednf x = S.singleton (S.singleton x)

negative :: Prop -> Bool
negative (Negation p) = True
negative _ = False

positive :: Prop -> Bool
positive = not . negative

negate' :: Prop -> Prop
negate' (Negation p) = p
negate' x = Negation x

trivial :: S.Set Prop -> Bool
trivial lits = let (pos,neg) = S.partition positive lits in
   S.intersection pos (S.map negate' neg) /= S.empty

simpdnf :: Prop -> S.Set (S.Set Prop)
simpdnf fm = let djs = S.filter (not . trivial) (purednf (nnf fm)) in
    S.filter (\d -> not (any (`S.isProperSubsetOf` d) djs)) djs

-- | sat checking based on simpdnf (set based dnf representation)
dnfsat :: [Prop] -> Bool
dnfsat p = not $ S.null $ simpdnf (foldr1 Conjunction p)

listdisj :: S.Set Prop -> Prop
listdisj s = if S.null s then Conjunction (Basic "A") (Negation (Basic "A"))
    else foldr1 Disjunction (S.elems s)

listconj :: S.Set Prop -> Prop
listconj s = if S.null s then Disjunction (Basic "A") (Negation (Basic "A"))
    else foldr1 Conjunction (S.elems s)

dnf :: Prop -> Prop
dnf fm = listdisj (S.map listconj (simpdnf fm))

purecnf :: Prop -> S.Set (S.Set Prop)
purecnf fm = S.map (S.map negate') (purednf (nnf (Negation fm)))

simpcnf :: Prop -> S.Set (S.Set Prop)
simpcnf fm = let cjs = S.filter (not. trivial) (purecnf fm) in
    S.filter (\x -> not (any (`S.isProperSubsetOf` x) cjs)) cjs

cnf :: Prop -> Prop
cnf fm = listconj (S.map listdisj (simpcnf fm))

-- | okay, here we go, davis-putnam

-- | first the "one literal rule" (this rule is correct)
oneliteralrule :: S.Set (S.Set Prop) -> Maybe (S.Set (S.Set Prop))
oneliteralrule clauses =
    let mu = setFind unarySet clauses in case mu of
      Nothing -> Nothing
      Just set -> let u = head $ S.toList set in
                  let u' = negate' u in
          let clauses1 = S.filter (not . S.member u) clauses in
        --      Just $ S.map (\x -> S.difference x (S.singleton u')) clauses1
              Just $ S.map (S.\\ S.singleton u') clauses1

unarySet :: S.Set Prop -> Bool
unarySet s = setSize s == 1

setFind ::(a -> Bool) -> S.Set a -> Maybe a
setFind p xs = L.find p $ S.toList xs

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

setSize :: S.Set a-> Int
setSize = S.foldr (\_ n -> 1 + n) 0

-- |second, the affirmative-negative rule (or the pure literal rule)

affirmativenegativerule :: S.Set (S.Set Prop) -> Maybe (S.Set (S.Set Prop))
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

resolveon :: Prop -> S.Set (S.Set Prop) -> Maybe (S.Set (S.Set Prop))
resolveon p clauses =
    let p' = negate' p in
    let (pos,notpos) = S.partition (S.member p) clauses in
    let (neg,other) = S.partition (S.member p') notpos in
    let pos' = S.map (S.filter (/= p)) pos in
    let neg' = S.map (S.filter (/= p')) neg in
    let res0 = S.fromList $ [S.union x y | x <- S.toList pos', y <- S.toList neg'] in
    Just $ S.union other (S.filter (not . trivial) res0)

-- | resolution blowup

resolutionblowup :: S.Set (S.Set Prop) -> Prop -> Int
resolutionblowup cls l =
    let m = setSize (S.filter (S.member l) cls) in
    let n = setSize (S.filter (S.member (negate' l)) cls) in
    m * n - m - n

-- | resolution rule

resolutionrule :: S.Set (S.Set Prop) -> Maybe (S.Set (S.Set Prop))
resolutionrule clauses =
    let pvs = S.filter positive (S.unions clauses) in
    let p = minimize (resolutionblowup clauses) pvs in
    resolveon p clauses

minimize :: (a -> Int) -> S.Set a -> a
minimize f xs = let ordindex = map f (S.toList xs) in
    fromJust $ L.lookup (minimum ordindex) (zip ordindex (S.toList xs))

-- the dp procedure

dp :: S.Set (S.Set Prop) -> Bool
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

dpsat :: [Prop] -> Bool
dpsat ps = dp (simpcnf (foldr1 Conjunction ps))

printSets :: S.Set (S.Set Prop) -> String
printSets s = "{" ++ L.intercalate "," (map printSet (S.toList s)) ++ "}"

printSet :: S.Set Prop -> String
printSet s = "[" ++ printprops (S.toList s) ++ "]"


dptrace :: S.Set (S.Set Prop) -> Writer H.Html ()
dptrace clauses = do
    if clauses == S.empty 
        then tell $ H.toHtml "concluded that the input is satisfiable." <> H5.br
        else if S.member S.empty clauses
            then tell $ H.toHtml "concluded that the input is not satisfiable." <> H5.br
            else case oneliteralrule clauses of
                 Nothing -> do
                     tell $ H.toHtml "couldn't apply the oneliteralrule" <> H5.br
                     tell $ H.toHtml ("    " ++ printSets clauses) <> H5.br
                     case affirmativenegativerule clauses of
                            Nothing -> do 
                                tell $ H.toHtml "couldn't apply the affirmative negative rule" <> H5.br
                                tell $ H.toHtml ("    " ++ printSets clauses) <> H5.br
                                case resolutionrule clauses of
                                     Nothing -> do tell $ H.toHtml "this shouldn't happen" <> H5.br
                                     Just set -> do
                                          tell $ H.toHtml "applied the resolution rule to get: " <> H5.br <>  H.toHtml (printSets set)  <> H5.br
                                          dptrace set
                            Just set -> do 
                                tell $ H.toHtml "applied the affirmative negative rule to get: " <> H5.br <>  H.toHtml (printSets set)  <> H5.br
                                dptrace set
                 Just set -> do
                     tell $ H.toHtml "applied the oneliteralrule rule to get: " <> H5.br <>  H.toHtml (printSets set)  <> H5.br
                     dptrace set

dodptrace :: [Prop] -> H.Html
dodptrace xs = snd $ runWriter $ dptrace $ simpcnf (foldr1 Conjunction xs)