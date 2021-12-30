module GPL where

import Data.Prop
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import qualified Data.Set as S

import qualified PL    --- functions for the propositional case

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

-- | universally quantify over all free variables in a formula
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
  _ -> PL.psimplify1 fm

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
pnf fm = prenex (PL.nnf (simplify fm))

-- skolemize

funcs :: Term -> [(String, Int)]
funcs tm = case tm of
    Var x -> []
    Fn f args -> foldr (union . funcs) [(f,length args)] args

functions :: Formula Fol -> [(String, Int)]
functions = atomunion (\(R _ args) -> foldr (union . funcs) [] args)

atomunion f fm = nub (PL.overatoms (\h t -> f h ++ t) fm [])

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
askolemize fm = fst ((skolem $ PL.nnf $ simplify fm) (map fst (functions fm)))

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
    let mfn djs0 ifn djs = S.filter (not . PL.trivial) (PL.distribS (S.map (S.map ifn) djs0) djs) in
            herbloop mfn (/= S.empty)

-- gilmore procedure

gilmore :: Formula Fol -> IO Int
gilmore fm =
    let sfm = skolemize (Not (generalize fm)) in
    let fvs = fv sfm in
    let (consts,funcs) = herbfuns sfm in
    let cntms = map (\(c,_) -> Fn c []) consts in
    do tms <- gilmoreloop (PL.simpdnfS sfm) cntms funcs fvs 0 (S.singleton S.empty) [] []
       return (length tms)

-- some test props

p45 = Imp (foldl1 And [p45a,p45b,p45c]) p45d
p45a = parser "@x((Fx&@y((Gy&Hxy)->Jxy))->@y((Gy&Hxy)->Ky))"
p45b = parser "~#y(Ly&Ky)"
p45c = parser "#x((Fx&@y(Hxy->Ly))&@y((Gy&Hxy)->Jxy))"
p45d = parser "#x(Fx&~#x(Gy&Hxy))"

p20 = parser "(@x@y#z@w((Px&Qy)->(Rz&Uw))->(#x#y(Px&Qy) -> #zRz))"

pdmfn cjs0 ifn = Set.union (Set.map (Set.map ifn) cjs0)

dploop = herbloop pdmfn PL.dpll

davisputnam :: Formula Fol -> IO Int
davisputnam fm =
    let sfm = skolemize (Not (generalize fm)) in
    let fvs = fv sfm in
    let (consts,funcs) = herbfuns sfm in
    let cntms = map (\(c,_)-> Fn c []) consts in
       do length <$> dploop (PL.simpcnfS sfm) cntms funcs fvs 0 S.empty [] []


dprefine cjs0 fvs dunno need =
    case dunno of
        [] -> need
        cl:dknow ->
            let mfn = pdmfn cjs0 . subst . Map.fromList . zip fvs in
            let need' = if PL.dpll (foldr mfn S.empty (need ++ dknow))
                            then cl:need
                            else need in
            dprefine cjs0 fvs dknow need'

dprefineloop cjs0 cntms funcs fvs n cjs tried tuples = do
    tups <- dploop cjs0 cntms funcs fvs n cjs tried tuples
    return (dprefine cjs0 fvs tups [])

davisputnam' :: Formula Fol -> IO Int
davisputnam' fm =
    let sfm = skolemize (Not (generalize fm)) in
    let fvs = fv sfm in
    let (consts,funcs) = herbfuns sfm in
    let cntms = map (\(c,_)-> Fn c []) consts in
       do length <$> dprefineloop (PL.simpcnfS sfm) cntms funcs fvs 0 S.empty [] []

-- UNIFICATION

istriv :: Map.Map String Term -> String -> Term -> Maybe Bool
istriv env x t = case t of
    Var y -> if x == y then Just True else
        case Map.lookup y env of
            Nothing -> Just False
            Just t' -> istriv env x t'
    Fn _ args ->
        if any triv args then Nothing else Just False
        where triv t' = case istriv env x t' of
                          Just False -> False
                          _ -> True

unify :: Map.Map String Term -> [(Term, Term)] -> Maybe (Map.Map String Term)
unify env eqs = case eqs of
    [] -> Just env
    ((Fn f fargs, Fn g gargs):rest) ->
        if f == g && length fargs == length gargs then
            unify env (zip fargs gargs ++ rest)
        else Nothing
    (Var x, t) : rest -> unifyVar x t rest
    (t, Var x) : rest -> unifyVar x t rest
    where unifyVar x t rest =
            case Map.lookup x env of
                  Just t' -> unify env ((t',t): rest)
                  Nothing -> case istriv env x t of
                    Just True -> unify env rest
                    Just False -> unify (Map.insert x t env) rest
                    Nothing -> Nothing

solve :: Map.Map String Term -> Maybe (Map.Map String Term)
solve env = solve' env (Map.toList env)


solve' :: Map.Map String Term -> [(String, Term)] -> Maybe (Map.Map String Term)
solve' env fs =
    if any (\(x,t) -> x `elem` fvt t) fs
    then Nothing else
    let env' = foldr (\(x,t) -> Map.insert x (tsubst env t)) Map.empty fs
        fs' = Map.toList env' in
    if fs == fs' then Just env else solve' env' fs'


fullunify :: [(Term, Term)] -> Maybe (Map.Map String Term)
fullunify eqs = do env <- unify Map.empty eqs
                   solve env

unifyAndApply :: [(Term,Term)] -> Maybe [(Term,Term)]
unifyAndApply eqs = 
    do env <- fullunify eqs
       let apply' (t1,t2) = (tsubst env t1, tsubst env t2) in
        return $ map apply' eqs

-- TABLEAUX

unifyliterals :: Map.Map String Term -> (Formula Fol, Formula Fol) -> Maybe (Map.Map String Term)
unifyliterals env tmp = 
    case tmp of
    (Atom (R p1 a1),Atom (R p2 a2)) -> unify env [(Fn p1 a1, Fn p2 a2)]
    (Not p, Not q) -> unifyliterals env (p,q) 
    (Bot,Bot) -> Just env
    _ -> Nothing

unifycomplements :: Map.Map String Term -> (Formula Fol, Formula Fol) -> Maybe (Map.Map String Term)
unifycomplements env (p,q) = unifyliterals env (p, PL.negate q)

unifyrefute :: [[Formula Fol]]-> Map.Map String Term -> Maybe (Map.Map String Term)
unifyrefute [] env = Just env
unifyrefute (d:odjs) env =
    let (pos, neg) = partition PL.positive d in
    let pairs = allpairs (,) pos neg in 
    let findFn pq = do env' <- unifycomplements env pq
                       unifyrefute odjs env' in
    findFirst findFn pairs

findFirst :: (t -> Maybe a) -> [t] -> Maybe a
findFirst _ [] = Nothing
findFirst f (x:xs) = case f x of
    Nothing -> findFirst f xs
    Just y -> Just y

prawitzloop :: [[Formula Fol]] -> [String] -> [[Formula Fol]] -> Int -> Maybe (Map.Map String Term, Int)
prawitzloop djs0 fvs djs n =
    let l = length fvs in
    let newvars = map (\k -> Var ("_" ++ show (n * l + k))) [1..l] in
    let inst = Map.fromList (zip fvs newvars) in
    let djs1 = PL.distrib (map (map (subst inst)) djs0) djs in
    case unifyrefute djs1 Map.empty of
        Just env -> Just (env, n+1)
        Nothing -> prawitzloop djs0 fvs djs1 (n+1)

prawitz :: Formula Fol -> Maybe Int
prawitz fm = 
    let fm0 = skolemize $ Not $ generalize fm in
    case prawitzloop (PL.simpdnf fm0) (fv fm0) [[]] 0 of
        Nothing -> Nothing
        Just (_,n) -> Just n

tableau (fms, lits, n) cont (env,k) =
    if n < 0 then Nothing else
      case fms of
        [] -> Nothing
        And p q : unexp -> tableau (p:q:unexp, lits, n) cont (env,k)
        Or p q : unexp -> tableau (p:unexp, lits, n) cont (env,k)
        fm@(Forall x p) : unexp ->
            let y = Var ("_" ++ show k)
                p' = subst (Map.singleton x y) p in
                     tableau (p':unexp ++ [fm], lits, n-1) cont (env, k+1)
        fm:unexp ->
            let findFn l = do env' <- unifycomplements env (fm,l)
                              cont (env',k) in
            case findFirst findFn lits of
                Just x -> Just x
                Nothing -> tableau (unexp, fm:lits, n) cont (env,k)

deepen f n = do
    putStrLn ("Searching with depth limit " ++ show n)
    case f n of
        Just x -> return x
        Nothing -> deepen f (n+1)

tabrefute fms =
    let tabFn n = do tableau (fms, [], n) Just (Map.empty, 0)
                     return n in
    deepen tabFn 0 

tab fm = 
    let sfm = askolemize $ Not $ generalize fm in
    if sfm == Bot then return 0 else tabrefute [sfm]