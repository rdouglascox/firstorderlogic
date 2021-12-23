module Fol where

import Data.Prop
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import qualified DPLL

import Parsing.Parser
import Printing.Printer

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

-- | substitutions

-- | substitution in terms
tsubst :: Map.Map String Term -> Term -> Term
tsubst sfn tm =
    case tm of
      Var x -> Data.Maybe.fromMaybe tm (Map.lookup x sfn)
      Fn f args -> Fn f (map (tsubst sfn) args)


-- | get a variant on a variable
variant :: String -> [String] -> String
variant x vars = if x `elem` vars
     then variant (x ++ "'") vars
     else x

-- | substitution in formulas
subst :: Map.Map String Term -> Formula Fol -> Formula Fol
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


substq :: Map.Map String Term -> (String -> Formula Fol -> Formula Fol) -> String -> Formula Fol -> Formula Fol
substq subfn quant x p =
    let x' = if any (\k -> case Map.lookup k subfn of
                                 Just v -> x `elem` fvt v
                                 Nothing -> False) (fv p \\ [x])
                then variant x (fv (subst (Map.delete x subfn) p))
                else x in
                    quant x' (subst (Map.insert x (Var x') subfn) p)

-- simplify

-- | remove vacuous quantifiers
simplify1 :: Formula Fol -> Formula Fol
simplify1 fm = case fm of
  Forall x p -> if x `elem` fv p then fm else p
  Exists x p -> if x `elem` fv p then fm else p
  _ -> psimplify1 fm

-- | propositional simplication
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
  Imp Top p -> p
  Imp p Bot -> Not p
  Iff p Top -> p
  Iff Top p -> p
  Iff p Bot -> Not p
  Iff Bot p -> Not p
  _ -> fm

-- simplify fol formula

simplify :: Formula Fol -> Formula Fol
simplify fm = case fm of
  Not p -> simplify1 (Not (simplify p))
  And p q -> simplify1 (And (simplify p) (simplify q))
  Or p q -> simplify1 (Or (simplify p) (simplify q))
  Imp p q -> simplify1 (Imp (simplify p) (simplify q))
  Iff p q -> simplify1 (Iff (simplify p) (simplify q))
  Forall s p -> simplify1 (Forall s (simplify p))
  Exists s p -> simplify1 (Exists s (simplify p))
  _ -> fm

-- negation normal form for fol

nnf :: Formula Fol -> Formula Fol
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
    Forall x p -> Forall x (nnf p)
    Exists x p -> Exists x (nnf p)
    Not (Forall x p) -> Exists x (nnf (Not p))
    Not (Exists x p) -> Forall x (nnf (Not p))
    _ -> fm

pullquants :: Formula Fol -> Formula Fol
pullquants fm = case fm of
    And (Forall x p) (Forall y q) -> pullq(True,True) fm Forall And x y p q
    Or (Exists x p) (Exists y q) -> pullq(True,True) fm Exists Or x y p q
    And (Forall x p) q -> pullq(True,False) fm Forall And x x p q
    And p (Forall y q) -> pullq(False,True) fm Forall And y y p q
    Or (Forall x p) q -> pullq(True,False) fm Forall Or x x p q
    Or p (Forall y q) -> pullq(False,True) fm Forall Or y y p q
    And (Exists x p) q -> pullq(True,False) fm Exists And x x p q
    And p (Exists y q) -> pullq(False,True) fm Exists And y y p q
    Or (Exists x p) q -> pullq(True,False) fm Exists Or x x p q
    Or p (Exists y q) -> pullq(False,True) fm Exists Or y y p q
    _ -> fm

pullq (l,r) fm quant op x y p q =
    let z = variant x (fv fm) in
    let p' = if l then subst (Map.singleton x (Var z)) p else p in
    let q' = if r then subst (Map.singleton x (Var z)) q else q in
    quant z (pullquants (op p' q'))

prenex :: Formula Fol -> Formula Fol
prenex fm = case fm of
  Forall x p -> Forall x (prenex p)
  Exists x p -> Exists x (prenex p)
  And p q -> pullquants (And (prenex p) (prenex q))
  Or p q -> pullquants (Or (prenex p) (prenex q))
  _ -> fm

-- |prenex normal form
pnf :: Formula Fol -> Formula Fol
pnf fm = prenex (nnf (simplify fm))

-- skolemize

funcs :: Term -> [(String, Int)]
funcs tm = case tm of
    Var x -> []
    Fn f args -> foldr (union . funcs) [(f,length args)] args

functions :: Formula Fol -> [(String, Int)]
functions = atomunion (\(R _ args) -> foldr (union . funcs) [] args)

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

atomunion f fm = nub (overatoms (\h t -> f h ++ t) fm [])

skolem :: Formula Fol -> [String] -> (Formula Fol, [String])
skolem fm fns =
    case fm of
        Exists y p ->
            let xs = fv fm in
            let f = variant (if null xs then "c_" ++ y else "f_" ++ y) fns in
            let fx = Fn f (map Var xs) in
            skolem (subst (Map.singleton y fx) p) (f:fns)
        Forall x p -> let (p',fns') = skolem p fns in (Forall x p', fns)
        And p q -> skolem2 And (p,q) fns
        Or p q-> skolem2 Or (p,q) fns
        _ -> (fm,fns)

skolem2 cons (p,q) fns =
    let (p',fns') = skolem p fns in
    let (q',fns'') = skolem q fns' in
    (cons p' q',fns'')


askolemize :: Formula Fol -> Formula Fol
askolemize fm = fst ((skolem $ nnf $ simplify fm) (map fst (functions fm)))

specialize :: Formula Fol -> Formula Fol
specialize fm =
    case fm of
        Forall x p -> specialize p
        _ -> fm

skolemize :: Formula Fol -> Formula Fol
skolemize fm = specialize (pnf(askolemize fm))

-- canonical models

pholds :: (Formula a -> Bool) -> Formula a -> Bool
pholds d fm = eval fm (d . Atom)

eval fm v = case fm of
  Bot -> False
  Top -> True
  Atom x -> v x
  Not p -> not (eval p v)
  And p q -> eval p v && eval q v
  Or p q -> eval p v || eval q v
  Imp p q -> not (eval p v) || eval q v
  Iff p q -> eval p v == eval q v
  _ -> error "tried to eval something not evaluable"

herbfuns :: Formula Fol -> ([(String, Int)], [(String, Int)])
herbfuns fm =
    let syms@(cns,fns) = partition ((== 0) . snd) (functions fm) in
        if null cns
            then ([("c",0)],fns)
            else syms

-- enumerate all ground terms involving n functions 

groundterms cntms _ 0 = cntms
groundterms cntms funcs n =
  let grtps (f, arity) =  map (Fn f) (groundtuples cntms funcs (n-1) arity) in
  concatMap grtps funcs


groundtuples _ _ 0 0 = [[]]
groundtuples _ _ _ 0 = []
groundtuples cntms funcs n m =
  let tups k = allpairs (:)
               (groundterms cntms funcs k)
               (groundtuples cntms funcs (n-k) (m-1)) in
  concatMap tups [0 .. n]

allpairs :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
allpairs f xs ys = [f x y | x <- xs, y <- ys]

herbloop mfn tfn f10 cntms funcs fvs n fl tried tuples = do
    putStrLn (show (length tried) ++ " ground instances tried; " ++ show (length fl) ++ " items in list")
    case tuples of
        [] -> let newtups = groundtuples cntms funcs n (length fvs) in
              herbloop mfn tfn f10 cntms funcs fvs (n+1) fl tried newtups
        tup:tups -> let fl' = mfn f10 (subst $ Map.fromList $ zip fvs tup) fl in
                    if not (tfn fl') then return $ tup:tried else
                    herbloop mfn tfn f10 cntms funcs fvs n fl' (tup:tried) tups

gilmoreloop =
    let mfn djs0 ifn djs = filter (not . wtrivial) (wdistrib' (map (map ifn) djs0) djs) in
            herbloop mfn (/= [])

wtrivial :: Ord a => [Formula a] -> Bool
wtrivial xs = DPLL.trivial (Set.fromList xs)

wdistrib' :: Ord a => [[Formula a]] -> [[Formula a]] -> [[Formula a]]
wdistrib' xs ys = unwrap (DPLL.distrib' (wrap xs) (wrap ys))

wrap :: Ord a => [[a]] -> Set.Set (Set.Set a)
wrap xs = Set.fromList (map Set.fromList xs)

unwrap :: Ord a => Set.Set (Set.Set a) -> [[a]]
unwrap xs = Set.toList (Set.map Set.toList xs)

-- gilmore procedure

gilmore :: Formula Fol -> IO Int
gilmore fm =
    let sfm = skolemize (Not (generalize fm)) in
    let fvs = fv sfm in
    let (consts,funcs) = herbfuns sfm in
    let cntms = map (\(c,_) -> Fn c []) consts in
    do tms <- gilmoreloop (wsimpdnf sfm) cntms funcs fvs 0 [[]] [] []
       return (length tms)

wsimpdnf :: Ord a => Formula a -> [[Formula a]]
wsimpdnf xs = unwrap (DPLL.simpdnf xs)

-- p45 = Imp (foldl1 And [p45a,p45b,p45c]) p45d
p45a = parser "@x((Fx&@y((Gy&Hxy)->Jxy))->@y((Gy&Hxy)->Ky))"
p45b = parser "~#y(Ly&Ky)"
p45c = parser "#x((Fx&@y(Hxy->Ly))&@y((Gy&Hxy)->Jxy))"
p45d = parser "#x(Fx&~#x(Gy&Hxy))"


pdmfn cjs0 ifn = union (map (map ifn) cjs0)

dploop :: [[Formula Fol]] -> [Term] -> [(String, Int)] -> [String] -> Integer -> [[Formula Fol]] -> [[Term]] -> [[Term]] -> IO [[Term]]
dploop = herbloop pdmfn wdpll

wdpll :: Ord a => [[Formula a]] -> Bool
wdpll xs = DPLL.dpll (wrap xs)

davisputnam :: Formula Fol -> IO Int
davisputnam fm =
    let sfm = skolemize (Not (generalize fm)) in
    let fvs = fv sfm in
    let (consts,funcs) = herbfuns sfm in
    let cntms = map (\(c,_)-> Fn c []) consts in
       do length <$> dploop (wsimpcnf sfm) cntms funcs fvs 0 [] [] []


wsimpcnf :: Ord a => Formula a -> [[Formula a]]
wsimpcnf xs = unwrap (DPLL.simpcnf xs)

p20 = parser "(@x@y#z@w((Px&Qy)->(Rx&Uw))->(#x#y(Px&Qy) -> #zRz))"

