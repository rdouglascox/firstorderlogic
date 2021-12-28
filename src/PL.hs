module PL where

import Data.Prop

import Data.List ( nub, union, partition, intersect, (\\) )

import Prelude hiding (negate)

import Parsing.BNFPLParser (parse)  -- for testing

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
    Not (Not p) -> p
    Not (And p q) -> Or (nnf (Not p)) (nnf (Not q))
    Not (Or p q) -> And (nnf (Not p)) (nnf (Not q))
    Not (Imp p q) -> And (nnf p) (nnf (Not q))
    Not (Iff p q) -> Or (And (nnf p) (nnf (Not q))) (And (nnf (Not p)) (nnf q))
--     Forall x p -> Forall x (nnf p)
--     Exists x p -> Exists x (nnf p)
--     Not (Forall x p) -> Exists x (nnf (Not p))
--     Not (Exists x p) -> Forall x (nnf (Not p))
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
distrib :: Formula a -> Formula a
distrib (And p (Or q r)) = Or (distrib (And p q)) (distrib (And p r))
distrib (And (Or p q) r) = Or (distrib (And p r)) (distrib (And q r))
distrib x = x

-- raw disjunctive normal form
rawdnf :: Formula a -> Formula a
rawdnf (And p q) = distrib (And (rawdnf p) (rawdnf q))
rawdnf (Or p q) = Or (rawdnf p) (rawdnf q)
rawdnf x = x

distrib' :: Eq a => [[Formula a]] -> [[Formula a]] -> [[Formula a]]
distrib' s1 s2 = nub (allpairs union s1 s2)

-- | apply a function to all pairs taken from two lists 
allpairs :: Eq a => (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
allpairs f xs ys = nub $ [f x y | x <- xs, y <- ys]

purednf :: Eq a => Formula a -> [[Formula a]]
purednf fm =
    case fm of
        And p q -> distrib' (purednf p) (purednf q)
        Or p q -> purednf p `union` purednf q
        _ -> [[fm]]

trivial :: Eq a => [Formula a] -> Bool
trivial lits =
    let (pos,neg) = partition positive lits in
    intersect pos (map negate neg) /= []

simpdnf :: Eq a => Formula a -> [[Formula a]]
simpdnf fm = let djs = filter (not . trivial) (purednf (nnf fm)) in
    filter (\d -> not (any (`isProperSubsetOf` d) djs)) djs

isProperSubsetOf :: Eq a => [a] -> [a] -> Bool
isProperSubsetOf xs ys = null (nub xs \\ nub ys) && nub xs /= nub ys

listdisj :: [Formula a] -> Formula a 
listdisj s = if null s then Top
    else foldr1 Or s 

listconj :: [Formula a] -> Formula a
listconj s = if null s then Bot 
    else foldr1 And s 

dnf :: Eq a => Formula a -> Formula a
dnf fm = listdisj (map listconj (simpdnf fm))

-- CONJUNCTIVE NORMAL FORM

purecnf :: Eq a => Formula a -> [[Formula a]]
purecnf fm = map (map negate) (purednf(nnf (Not fm)))

simpcnf :: Eq a => Formula a -> [[Formula a]]
simpcnf fm = let cjs = filter (not. trivial) (purecnf fm) in
    filter (\x -> not (any (`isProperSubsetOf` x) cjs)) cjs

cnf :: Eq a => Formula a -> Formula a
cnf fm = listconj (map listdisj (simpcnf fm))

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