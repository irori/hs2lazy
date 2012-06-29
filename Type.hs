-- Part of `Typing Haskell in Haskell', version of November 23, 2000
-- Copyright (c) Mark P Jones and the Oregon Graduate Institute
-- of Science and Technology, 1999-2000
-- 
-- This program is distributed as Free Software under the terms
-- in the file "License" that is included in the distribution
-- of this software, copies of which may be obtained from:
--             http://www.cse.ogi.edu/~mpj/thih/
--
--  modified by irori <irorin@gmail.com>

module Type where
import Data.List(nub, (\\), intersect, union, partition)
import Control.Monad(msum)
import Syntax

enumId  :: Int -> Id
enumId n = "v" ++ show n

-- Substitutions
nullSubst  :: Subst
nullSubst   = []

(+->)      :: Tyvar -> Type -> Subst
u +-> t     = [(u, t)]

infixr 4 @@
(@@)       :: Subst -> Subst -> Subst
s1 @@ s2    = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

merge      :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1++s2) else fail "merge fails"
 where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                   (map fst s1 `intersect` map fst s2)

-- Unification
mgu     :: Monad m => Type -> Type -> m Subst
varBind :: Monad m => Tyvar -> Type -> m Subst

mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
                               s2 <- mgu (apply s1 r) (apply s1 r')
                               return (s2 @@ s1)
mgu (TVar u) t        = varBind u t
mgu t (TVar u)        = varBind u t
mgu (TSynonym s ts) u = mgu (unsynonym s ts) u
mgu t (TSynonym s ts) = mgu t (unsynonym s ts)
mgu (TCon tc1) (TCon tc2)
           | tc1==tc2 = return nullSubst
mgu t1 t2             = fail ("types do not unify: "
			      ++ show t1 ++ " " ++ show t2)

varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = fail "occurs check fails"
            | kind u /= kind t = fail "kinds do not match"
            | otherwise        = return (u +-> t)

match :: Monad m => Type -> Type -> m Subst

match (TAp l r) (TAp l' r') = do sl <- match l l'
                                 sr <- match r r'
                                 merge sl sr
match (TVar u)   t | kind u == kind t = return (u +-> t)
match (TSynonym s ts) u = match (unsynonym s ts) u
match t (TSynonym s ts) = match t (unsynonym s ts)
match (TCon tc1) (TCon tc2)
         | tc1==tc2         = return nullSubst
match t1 t2                 = fail "types do not match"

-----------------------------------------------------------------------------
-- Pred:		Predicates
-----------------------------------------------------------------------------

mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred             = lift mgu
matchPred           = lift match

lift m (IsIn i t) (IsIn i' t')
         | i == i'   = m t t'
         | otherwise = fail "classes differ"

-----------------------------------------------------------------------------

super     :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of
               Just (is, its, ms) -> is
               Nothing -> error ("super " ++ i)

insts     :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of Just (is, its, ms) -> its

methods   :: ClassEnv -> Id -> [Assump]
methods ce i = case classes ce i of Just (is, its, ms) -> ms

defined :: Maybe a -> Bool
defined (Just x) = True
defined Nothing  = False

modify       :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce{classes = \j -> if i==j then Just c
                                           else classes ce j}

initialEnv :: ClassEnv
initialEnv  = ClassEnv { classes  = \i -> fail "class not defined",
                         defaults = [],
                         impls    = [],
                         expls    = [],
                         assumps  = [] }

addClass :: Id -> [Id] -> [Assump] -> EnvTransformer
addClass i is ms ce
 | defined (classes ce i)              = fail "class already defined"
 | any (not . defined . classes ce) is = fail "superclass not defined"
 | otherwise = return (modify (ce{assumps = assumps ce ++ ms}) i (is, [], ms)) 

addInst :: [Pred] -> Pred -> Expr -> EnvTransformer
addInst ps p@(IsIn i _) dict ce
 | not (defined (classes ce i)) = error ("no class for instance " ++ i)
 | any (overlap p) qs           = error ("overlapping instance " ++ i)
 | otherwise                    = return (modify ce i c)
   where its = insts ce i
         qs  = [ q | (_ :=> q, _) <- its ]
         c   = (super ce i, (ps:=>p, dict) : its, methods ce i)

addImpls :: [Impl] -> EnvTransformer
addImpls is ce = return (ce { impls = impls ce ++ is })

addExpls :: [Expl] -> EnvTransformer
addExpls es ce = return (ce { expls = expls ce ++ es })

addAssumps :: [Assump] -> EnvTransformer
addAssumps is ce = return (ce { assumps = assumps ce ++ is })

overlap       :: Pred -> Pred -> Bool
overlap p q    = defined (mguPred p q)

{-
exampleInsts ::  EnvTransformer
exampleInsts =   addPreludeClasses
             <:> addInst [] (IsIn "Ord" tUnit)
             <:> addInst [] (IsIn "Ord" tChar)
             <:> addInst [] (IsIn "Ord" tInt)
             <:> addInst [IsIn "Ord" (TVar (Tyvar "a" Star)),
                          IsIn "Ord" (TVar (Tyvar "b" Star))]
                         (IsIn "Ord" (pair (TVar (Tyvar "a" Star))
                                           (TVar (Tyvar "b" Star))))
-}

-----------------------------------------------------------------------------

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t)
 = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

byInst  :: ClassEnv -> Pred -> Maybe ([Pred], Expr)
byInst ce p@(IsIn i t)    = msum [ tryInst it | it <- insts ce i ]
 where tryInst (ps :=> h, dict) = do u <- matchPred h p
                                     Just (map (apply u) ps, dict)

entail        :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
                 case byInst ce p of
                   Nothing -> False
                   Just (qs, _) -> all (entail ce ps) qs

-----------------------------------------------------------------------------

inHnf       :: Pred -> Bool
inHnf (IsIn c t) = hnf t
 where hnf (TVar v)  = True
       hnf (TCon tc) = False
       hnf (TAp t _) = hnf t
       hnf (TSynonym s ts) = hnf (unsynonym s ts)

toHnfs      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf                 :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | inHnf p   = return [p]
           | otherwise = case byInst ce p of
                           Nothing -> fail ("context reduction " ++ show p)
                           Just (ps, _) -> toHnfs ce ps

simplify   :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
 where loop rs []                            = rs
       loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
                      | otherwise            = loop (p:rs) ps

reduce      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  return (simplify ce qs)

scEntail        :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)


-- Type inference monad
newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

instance Monad TI where
  return x   = TI (\s n -> (s,n,x))
  TI f >>= g = TI (\s n -> case f s n of
                            (s',m,x) -> let TI gx = g x
                                        in  gx s' m)

runTI       :: TI a -> a
runTI (TI f) = x where (s,n,x) = f nullSubst 0

getSubst   :: TI Subst
getSubst    = TI (\s n -> (s,n,s))

unify      :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst   :: Subst -> TI ()
extSubst s' = TI (\s n -> (s'@@s, n, ()))

newTVar    :: Kind -> TI Type
newTVar k   = TI (\s n -> let v = Tyvar (enumId n) k
                          in  (s, n+1, TVar v))

freshInst               :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

class Instantiate t where
  inst  :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst ts t         = t
instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

-----------------------------------------------------------------------------
-- TIMain:	Type Inference Algorithm
-----------------------------------------------------------------------------

type RecAssump = (Id, Type)
data Env = Env [Assump] [RecAssump]

instance Types Env where
  apply s (Env as ras) = Env (apply s as) [(i, apply s t) | (i, t) <- ras]
  tv (Env as ras)      = tv as `union` tv (map snd ras)

makeEnv :: [Assump] -> Env
makeEnv as = Env as []

extend :: Env -> [Assump] -> Env
extend (Env as ras) as' = Env (as' ++ as) ras

extendRec :: Env -> [RecAssump] -> Env
extendRec (Env as ras) ras' = Env as (ras' ++ ras)

lookupEnv :: Monad m => Env -> Id -> m (Either Scheme Type)
lookupEnv (Env as ras) i =
    case lookup i ras of
      Just t  -> return (Right t)
      Nothing -> find as
        where find []             = fail ("unbound identifier: " ++ i)
              find ((i':>:sc):as) = if i==i' then return (Left sc) else find as

-- Basic definitions for type inference
type Infer e t = ClassEnv -> Env -> e -> TI ([Pred], t, e)

-- Lit:		Literals
tiLit            :: Literal -> TI ([Pred], Type)
tiLit (LitChar _) = return ([], tChar)
tiLit (LitInt _)  = return ([], tInt)
tiLit (LitStr _)  = return ([], tString)

-- Pat:		Patterns
tiPat :: Pat -> TI ([Pred], [Assump], Type)

tiPat (PVar i) = do v <- newTVar Star
                    return ([], [i :>: toScheme v], v)

tiPat PWildcard   = do v <- newTVar Star
                       return ([], [], v)

tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
                       return (ps, (i:>:toScheme t):as, t)

tiPat (PLit l) = do (ps, t) <- tiLit l
                    return (ps, [], t)

tiPat (PCon con pats)
    = do (ps, as, ts) <- tiPats pats
	 t'           <- newTVar Star
	 (qs :=> t)   <- freshInst (conScheme con)
	 unify t (foldr fn t' ts)
	 return (ps ++ qs, as, t')

tiPats     :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do psasts <- mapM tiPat pats
                 let ps = concat [ ps' | (ps',_,_) <- psasts ]
                     as = concat [ as' | (_,as',_) <- psasts ]
                     ts = [ t | (_,_,t) <- psasts ]
                 return (ps, as, ts)

-----------------------------------------------------------------------------

tiExpr :: Infer Expr Type

tiExpr ce env e@(Var i) =
    do sc_or_t    <- lookupEnv env i
       case sc_or_t of
         Left sc -> do (ps :=> t) <- freshInst sc
                       return (ps, t, foldl Ap e (map ClassPH ps))
         Right t -> return ([], t, RecPH i)

tiExpr ce env e@(Con con)   = do (ps :=> t) <- freshInst (conScheme con)
			         return (ps, t, e)
tiExpr ce env e@(Lit l)     = do (ps, t) <- tiLit l
                                 return (ps, t, e)
tiExpr ce env (Ap e f)      = do (ps, te, e') <- tiExpr ce env e
                                 (qs, tf, f') <- tiExpr ce env f
                                 t            <- newTVar Star
                                 unify (tf `fn` t) te
                                 return (ps ++ qs, t, Ap e' f')
tiExpr ce env (Let bg e)    = do (ps, as, bg') <- tiBindGroup ce env bg
                                 (qs, t, e')   <- tiExpr ce (extend env as) e
                                 return (ps ++ qs, t, Let bg' e')

tiExpr ce env (Case e pses) = do (ps, te, e') <- tiExpr ce env e
			         tf           <- newTVar Star
			         t            <- newTVar Star
			         unify (te `fn` t) tf
			         (qs, alts')  <- tiAlts ce env alts tf
                                 let pses' = zip (map fst pses) (map snd alts')
			         return (ps ++ qs, t, Case e' pses')
    where alts = [([p], e) | (p, e) <- pses]

tiExpr ce env (Lambda alt)  = do (ps, t, alt') <- tiAlt ce env alt
                                 return (ps, t, Lambda alt')

--  e :: sc => let v :: sc; v = e in v と変換したときと同じになってる?
tiExpr ce env (ESign e sc) =
    do (qs :=> t)   <- freshInst sc
       (ps, te, e') <- tiExpr ce env e
       unify te t
       s          <- getSubst
       let qs' = apply s qs
           t'  = apply s t
	   fs  = tv (apply s env)
	   gs  = tv t' \\ fs
	   sc' = quantify gs (qs' :=> t')
           ps' = filter (not . entail ce qs') (apply s ps)
       (ds, rs) <- split ce fs gs ps'
       if sc /= sc'
        then fail "signature too general"
        else if not (null rs)
              then fail "context too weak"
              else return (ds, te, e')
       

-----------------------------------------------------------------------------

tiAlt :: Infer Alt Type
tiAlt ce env (pats, rhs) =
    do (ps, as, ts) <- tiPats pats
       (qs, t, rhs')  <- tiRhs ce (extend env as) rhs
       return (ps ++ qs, foldr fn t ts, (pats, rhs'))

tiAlts          :: ClassEnv -> Env -> [Alt] -> Type -> TI ([Pred], [Alt])
tiAlts ce env alts t = do r <- mapM (tiAlt ce env) alts
                          mapM (unify t) [t' | (_, t', _) <- r]
                          return (concat [p | (p, _, _) <- r],
                                  [a | (_, _, a) <- r])

-----------------------------------------------------------------------------

tiRhs :: Infer Rhs Type

tiRhs ce env (Rhs e) =
    do (ps, t, e') <- tiExpr ce env e
       return (ps, t, Rhs e')

tiRhs ce env (Where bg rhs) =
    do (ps, as, bg') <- tiBindGroup ce env bg
       (qs, t, rhs') <- tiRhs ce (extend env as) rhs
       return (ps ++ qs, t, Where bg' rhs')

tiRhs ce env (Guarded guards) =
    do t <- newTVar Star
       r <- mapM (tiGuard ce env) guards
       mapM (unify t) [t' | (_,t',_) <- r]
       return (concat [p | (p,_,_) <- r], t, Guarded [g| (_,_,g) <- r])

tiGuard :: Infer (Expr, Expr) Type
tiGuard ce env (cond, e) =
    do (ps, tcond, cond') <- tiExpr ce env cond
       unify tcond tBool
       (qs, te, e') <- tiExpr ce env e
       return (ps ++ qs, te, (cond', e'))

-----------------------------------------------------------------------------

split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
                      -> m ([Pred], [Pred])
split ce fs gs ps = do ps' <- reduce ce ps
                       let (ds, rs) = partition (all (`elem` fs) . tv) ps'
                       rs' <- defaultedPreds ce (fs++gs) rs
                       return (ds, rs \\ rs')

type Ambiguity       = (Tyvar, [Pred])

ambiguities         :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities ce vs ps = [ (v, filter (elem v . tv) ps) | v <- tv ps \\ vs ]

numClasses :: [Id]
numClasses  = ["Num", "Integral", "Floating", "Fractional",
               "Real", "RealFloat", "RealFrac"]

stdClasses :: [Id]
stdClasses  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
               "Functor", "Monad", "MonadPlus"] ++ numClasses

candidates           :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) = [ t' | let is = [ i | IsIn i t <- qs ]
                                   ts = [ t | IsIn i t <- qs ],
                               all ((TVar v)==) ts,
                               any (`elem` numClasses) is,
                               all (`elem` stdClasses) is,
                               t' <- defaults ce,
                               all (entail ce []) [ IsIn i t' | i <- is ] ]

withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a)
                  -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
    | any null tss  = fail "cannot resolve ambiguity"
    | otherwise     = return (f vps (map head tss))
      where vps = ambiguities ce vs ps
            tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds  = withDefaults (\vps ts -> concat (map snd vps))

defaultSubst   :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst    = withDefaults (\vps ts -> zip (map fst vps) ts)

-----------------------------------------------------------------------------
-- Resolving Placeholders

data ResolveEnv = ResolveEnv { reParam :: [(Pred, Expr)],
                               reRec   :: [(Id, Expr)],
                               reSubst :: Subst,
                               reClass :: ClassEnv }

resolve :: ClassEnv -> Subst -> [(Id, [Pred])] -> [Pred] -> [Alt] -> [Alt]
resolve ce s recs ps alts = map resolveAlt alts
    where dictVars = [c ++ '#' : v | IsIn c (TVar (Tyvar v _)) <- ps]
          env = ResolveEnv { reParam = zip ps (map Var dictVars),
                             reRec = [(i, foldl Ap (Var i) (map ClassPH ps))
                                      | (i, ps) <- recs],
                             reSubst = s,
                             reClass = ce }
          dictParams = map PVar dictVars
          resolveAlt (pats, rhs) = (dictParams ++ pats, resolveRhs env rhs)

resolveRhs :: ResolveEnv -> Rhs -> Rhs
resolveRhs re (Rhs e) = Rhs (resolveExpr re e)
resolveRhs re (Where bg rhs) =
    Where (resolveBindGroup re bg) (resolveRhs re rhs)
resolveRhs re (Guarded guards) =
    Guarded [(resolveExpr re cond, resolveExpr re e) | (cond, e) <- guards]

resolveExpr :: ResolveEnv -> Expr -> Expr
resolveExpr re e@(Var _) = e
resolveExpr re e@(Lit _) = e
resolveExpr re e@(Con _) = e
resolveExpr re (Ap e f) = Ap (resolveExpr re e) (resolveExpr re f)
resolveExpr re (Let bg e) = Let (resolveBindGroup re bg) (resolveExpr re e)
resolveExpr re (Case e pairs) =
    Case (resolveExpr re e) [(p, resolveRhs re rhs) | (p, rhs) <- pairs]
resolveExpr re (Lambda (pats, rhs)) = Lambda (pats, resolveRhs re rhs)
resolveExpr re (ESign e sc) = ESign (resolveExpr re e) sc
resolveExpr re e@(RecPH i) = case lookup i (reRec re) of
                                 Just e' -> resolveExpr re e'
                                 Nothing -> e
resolveExpr re e@(ClassPH p@(IsIn _ v)) =
    case lookup p' (reParam re) of
      Just e' -> e'
      Nothing ->
          case byInst (reClass re) p' of
            Just (ps, e') -> foldl Ap e' (map (resolveExpr re . ClassPH) ps)
            Nothing ->
                case resolveSuper re pes p' of
                  Just e' -> e'
                  Nothing -> e
    where p' = apply (reSubst re) p
          pes = [pe | pe@(IsIn _ v', _) <- reParam re, v == v']

resolveSuper :: ResolveEnv -> [(Pred, Expr)] -> Pred -> Maybe Expr
resolveSuper re [] p = Nothing
resolveSuper re pes@(_:_) p =
    case lookup p pes' of
      Just e' -> Just e'
      Nothing -> resolveSuper re pes' p
    where pes' = [(IsIn sup v, Var (cls ++ ">>" ++ sup) `Ap` e)
                  | (IsIn cls v, e) <- pes, sup <- super (reClass re) cls]

resolveBindGroup re (es, iss) = (es', iss')
    where es'  = [(i, sc, resolveAlts alts) | (i, sc, alts) <- es]
          iss' = map (\is -> [(i, resolveAlts alts) | (i, alts) <- is]) iss
          resolveAlts alts = [(pats, resolveRhs re rhs) | (pats, rhs) <- alts]
-----------------------------------------------------------------------------

tiExpl :: ClassEnv -> Env -> Expl -> TI ([Pred], Expl)
tiExpl ce env (i, sc, alts)
        = do (qs :=> t)  <- freshInst sc
             (ps, alts') <- tiAlts ce env alts t
             s           <- getSubst
             let qs'     = apply s qs
                 t'      = apply s t
                 fs      = tv (apply s env)
                 gs      = tv t' \\ fs
                 sc'     = quantify gs (qs' :=> t')
                 ps'     = filter (not . entail ce qs') (apply s ps)
                 alts''  = resolve ce s [] qs' alts'
             (ds, rs)   <- split ce fs gs ps'
             if sc /= sc' then
                 fail ("signature too general: expected" ++ show sc
                       ++ ", but inferred " ++ show sc')
               else if not (null rs) then
                 fail "context too weak"
               else
                 return (ds, (i, sc, alts''))

-----------------------------------------------------------------------------

restricted   :: [Impl] -> Bool
restricted bs = any simple bs
 where simple (i,alts) = any (null . fst) alts

tiImpls         :: Infer [Impl] [Assump]
tiImpls ce env [] = return ([], [], [])
tiImpls ce env bs =
    do ts <- mapM (\_ -> newTVar Star) bs
       let is    = map fst bs
           env'  = extendRec env (zip is ts)
           altss = map snd bs
       pssass <- sequence (zipWith (tiAlts ce env') altss ts)
       s      <- getSubst
       let ps'     = apply s (concat (map fst pssass))
           ts'     = apply s ts
           fs      = tv (apply s env)
           vss     = map tv ts'
           gs      = foldr1 union vss \\ fs
       (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
       if restricted bs then
           let gs'    = gs \\ tv rs
               scs    = map (quantify gs' . ([] :=>)) ts'
               recenv = zip is (repeat [])
               altss' = map (resolve ce s recenv [] . snd) pssass
               bs'    = zip is altss'
           in return (ds ++ rs, zipWith (:>:) is scs, bs')
         else
           let scs    = map (quantify gs . (rs :=>)) ts'
               recenv = zip is (repeat rs)
               altss' = map (resolve ce s recenv rs . snd) pssass
               bs'    = zip is altss'
           in return (ds, zipWith (:>:) is scs, bs')

-----------------------------------------------------------------------------

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce env (es,iss) =
  do let as = [ v:>:sc | (v,sc,alts) <- es ]
     (ps, as', iss') <- tiSeq tiImpls ce (extend env as) iss
     qses_s          <- mapM (tiExpl ce (extend env (as'++as))) es
     return (ps ++ concat (map fst qses_s), as' ++ as, (map snd qses_s, iss'))

tiSeq                  :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce env []       = return ([], [], [])
tiSeq ti ce env (bs:bss) = do (ps, as, bs')   <- ti ce env bs
                              (qs, as', bss') <- tiSeq ti ce (extend env as) bss
                              return (ps ++ qs, as' ++ as, bs':bss')


-- Type Inference for Whole Programs
tiProgram :: ClassEnv -> [Assump] -> Program -> ([Assump], Program)
tiProgram ce as bgs = runTI $
                      do (ps, as', bgs') <- tiSeq tiBindGroup ce (makeEnv as) bgs
                         s         <- getSubst
                         rs        <- reduce ce (apply s ps)
                         s'        <- defaultSubst ce [] rs
                         return (apply (s'@@s) as', bgs')

-----------------------------------------------------------------------------

preludeAssumptions :: [Assump]
preludeAssumptions = [
 "+"  :>: (toScheme (tInt `fn` tInt `fn` tInt)),
 "-"  :>: (toScheme (tInt `fn` tInt `fn` tInt)),
 "*"  :>: (toScheme (tInt `fn` tInt `fn` tInt)),
-- "/"  :>: (toScheme (tInt `fn` tInt `fn` tInt)),
 "div":>: (toScheme (tInt `fn` tInt `fn` tInt)),
 "mod":>: (toScheme (tInt `fn` tInt `fn` tInt)),
-- "==" :>: (toScheme (tInt `fn` tInt `fn` tBool)),
-- "eql" :>: (quantifyAll ([IsIn "Eq" a] :=> (a `fn` a `fn` tBool))),
-- "/=" :>: (toScheme (tInt `fn` tInt `fn` tBool)),
 "<"  :>: (toScheme (tInt `fn` tInt `fn` tBool)),
 ">"  :>: (toScheme (tInt `fn` tInt `fn` tBool)),
 "<=" :>: (toScheme (tInt `fn` tInt `fn` tBool)),
 ">=" :>: (toScheme (tInt `fn` tInt `fn` tBool)),
 "&&" :>: (toScheme (tBool `fn` tBool `fn` tBool)),
 "||" :>: (toScheme (tBool `fn` tBool `fn` tBool)),
 "ord":>: (toScheme (tChar `fn` tInt)),
 "chr":>: (toScheme (tInt `fn` tChar)),
 "++" :>: (quantifyAll' (list a `fn` list a `fn` list a)),
 "."  :>: (quantifyAll' ((b `fn` c) `fn` (a `fn` b) `fn` a `fn` c)),
 "error" :>: (quantifyAll' (list tChar `fn` a)),
 "hGetContents" :>: (toScheme (tInt `fn` list tChar)),
 "IF" :>: (quantifyAll' (tBool `fn` a `fn` a `fn` a)),
 "SEL" :>: (quantifyAll' (a `fn` b))]
    where a = TVar (Tyvar "a" Star)
	  b = TVar (Tyvar "b" Star)
	  c = TVar (Tyvar "c" Star)

addCoreClasses ::   EnvTransformer
addCoreClasses  = foldl1 (<:>) [
   addClass "Eq" [] [
        "==" :>: (quantifyAll ([IsIn "Eq" a] :=> (a `fn` a `fn` tBool))),
        "/=" :>: (quantifyAll ([IsIn "Eq" a] :=> (a `fn` a `fn` tBool)))],
   addImpls [tupleSelector "==" 0 2, tupleSelector "/=" 1 2],
--   addClass "Ord" ["Eq"] [],
--   addClass "Show" [] [],
--   addClass "Read" [] [],
--   addClass "Bounded" [] [],
--   addClass "Enum" [] [],
--   addClass "Functor" [] [],
--   addClass "Monad" [] [],
   addInst [] (IsIn "Eq" tInt) (Var "EqInt"),
   addImpls [("EqInt", [([], Rhs $ tuple [Var "primEq", Var "primNeq"])])],
   addInst [] (IsIn "Eq" tChar) (Var "EqChar"),
   addImpls [("EqChar", [([], Rhs $ tuple [Var "primEq", Var "primNeq"])])]
  ]
    where a = TVar (Tyvar "a" Star)

{-
addNumClasses  ::   EnvTransformer
addNumClasses   =   addClass "Num" ["Eq", "Show"]
                <:> addClass "Real" ["Num", "Ord"]
                <:> addClass "Fractional" ["Num"]
                <:> addClass "Integral" ["Real", "Enum"]
                <:> addClass "RealFrac" ["Real", "Fractional"]
                <:> addClass "Floating" ["Fractional"]
                <:> addClass "RealFloat" ["RealFrac", "Floating"]

addPreludeClasses :: EnvTransformer
addPreludeClasses  = addCoreClasses <:> addNumClasses
-}
