module Static where
import Data.List(union, partition, (\\))
import qualified Data.List(find)
import Syntax
import qualified Parser as P
import qualified Type as T
import PatComp (patBindings)

type TyconEnv = ([Tycon], [Synonym])
type DataType = (TyconEnv, [Const])

analyze :: ClassEnv -> [P.TopDecl] -> (Program, [Impl], ClassEnv, [Assump])
analyze ce tds = (prog, impls ce', ce', assumps ce')
    where (dataDecls, typeDecls, classDecls, instDecls,
           varDecls, patBinds, sigDecls)
              = splitDecls tds
          dt = anDataDecls synonyms dataDecls
          synonyms = map (anTypeDecl dt) typeDecls
          envTrans = anClassDecls dt classDecls <:> anInstDecls dt instDecls
          Just ce' = envTrans ce
	  bs = anVarDecls dt varDecls ++ anPatBinds dt patBinds
	  sigs = concat (map (anSigDecl dt) sigDecls)
	  (main, bs') = partition ((== "@main") . fst) bs
          prog = makeBindGroups sigs bs' ++
                 [(expls ce', []), makeBindGroup sigs main]

splitDecls tds = (dataDecls, typeDecls, classDecls, instDecls,
                  varDecls, patBinds, sigDecls)
    where dataDecls = [dd | P.DataDecl dd <- tds]
          typeDecls = [td | P.TypeDecl td <- tds]
          classDecls = [cd | P.ClassDecl cd <- tds]
          instDecls = [id | P.InstDecl id <- tds]
	  decls = [d | P.Decl d <- tds]
          varDecls = [vd | P.VarDecl vd <- decls]
          patBinds = [pb | P.PatBind pb <- decls]
          sigDecls = [sd | P.SigDecl sd <- decls]

anDecls :: DataType -> [P.Decl] -> [BindGroup]
anDecls dt decls = makeBindGroups sigs bs
    where bs   = collectBinds vds ++ pbs
          vds  = [anVarDecl dt a | P.VarDecl a <- decls]
          pbs  = anPatBinds dt [pe | P.PatBind pe <- decls]
	  sigs = concat [anSigDecl dt sd | P.SigDecl sd <- decls]

anVarDecls :: DataType -> [(String, [P.Pat], P.Rhs)] -> [Impl]
anVarDecls dt vds = collectBinds $ map (anVarDecl dt) vds

anVarDecl :: DataType -> (String, [P.Pat], P.Rhs) -> (Id, Alt)
anVarDecl dt (i, ps, rhs) = (i, alt)
    where alt = (map (anPat dt) ps, anRhs dt rhs)

anRhs :: DataType -> P.Rhs -> Rhs
anRhs dt rhs = foldr Where r (anDecls dt ds)
    where (r, ds) = case rhs of
                   P.Rhs e ds -> (Rhs $ anExpr dt e, ds)
                   P.Guarded gds ds -> (Guarded [(anExpr dt e, anExpr dt e')
                                                 | (e, e') <- gds], ds)

anPatBinds :: DataType -> [(P.Pat, P.Rhs)] -> [Impl]
anPatBinds dt pes = concat [anPatBind dt n pe | (pe, n) <- zip pes [1..]]

anPatBind :: DataType -> Int -> (P.Pat, P.Rhs) -> [Impl]
anPatBind dt n (p, rhs) = conf : bs
    where conf = (newvar, [([], anRhs dt $ makeBindRhs p rhs)])
	  bs   = patBindings (Var newvar) (anPat dt p)
	  newvar = "Tmp#" ++ show n

makeBindRhs :: P.Pat -> P.Rhs -> P.Rhs
makeBindRhs p (P.Rhs e ds) = P.Rhs e' ds
    where e' = P.Case e [(P.AsPat "@u" p, P.Rhs (P.Var "@u") [])]
makeBindRhs p (P.Guarded gds ds) = P.Guarded gds' ds
    where gds' = [(cond, P.Case e [(P.AsPat "@u" p, P.Rhs (P.Var "@u") [])])
                  | (cond, e) <- gds]

anSigDecl :: DataType -> ([Id], P.Context, P.Type) -> [(Id, Scheme)]
anSigDecl (tce, _) (is, ctx, t) = [(i, sc) | i <- is]
    where sc = quantifyAll (anContext ctx :=> anType tce t)

makeBindGroups :: [(Id, Scheme)] -> [Impl] -> [BindGroup]
makeBindGroups sigs bs = map (makeBindGroup sigs) (dependency bs)

makeBindGroup :: [(Id, Scheme)] -> [Impl] -> BindGroup
makeBindGroup sigs bs = (expls, [impls])
    where bs' = map makeBind bs
	  impls = [impl | Left impl <- bs']
	  expls = [expl | Right expl <- bs']
	  makeBind :: Impl -> Either Impl Expl
	  makeBind (i, alts) = case lookup i sigs of
			       Just sc -> Right (i, sc, alts)
			       Nothing -> Left (i, alts)

collectBinds :: [(Id, Alt)] -> [Impl]
collectBinds [] = []
collectBinds bs = (id, map snd bs1) : rest
    where (id, _) = head bs
	  (bs1, bs2) = partition ((== id) . fst) bs
	  rest = collectBinds bs2

anExpr :: DataType -> P.Expr -> Expr
anExpr dt (P.Var i) = Var i
anExpr (_, cs) (P.Con i) =
    case lookupConst i cs of
    Just con -> Con con
    Nothing -> error ("Undefined constructor function " ++ i)
anExpr dt (P.Tuple es) = ap (Con (tupcon (length es))) (map (anExpr dt) es)
anExpr dt (P.LitInt n) = Lit (LitInt n)
anExpr dt (P.LitChar c) = Lit (LitChar c)
anExpr dt (P.LitStr s) = foldr (\c e -> ap eCons [Lit $ LitChar [c], e]) eNil s
anExpr dt (P.Ap e1 e2) = Ap (anExpr dt e1) (anExpr dt e2)
anExpr dt (P.Let ds e) = foldr Let (anExpr dt e) (anDecls dt ds)
anExpr dt (P.Lambda ps e) =
    Lambda (map (anPat dt) ps, Rhs $ anExpr dt e)
anExpr dt@(tce, _) (P.ESign e ctx t) =
    ESign (anExpr dt e) (quantifyAll (anContext ctx :=> anType tce t))
anExpr dt (P.Case e as) =
    Case (anExpr dt e) [(anPat dt p, anRhs dt rhs) | (p, rhs) <- as]

anPat :: DataType -> P.Pat -> Pat
anPat dt (P.PVar i) = PVar i
anPat dt@(_, cs) (P.PCon i ps) =
    case lookupConst i cs of
    Just con -> PCon con (map (anPat dt) ps)
    Nothing -> error ("Undefined constructor function " ++ i)
anPat dt (P.PTuple ps) = PCon (tupcon (length ps)) (map (anPat dt) ps)
anPat dt (P.PInt n) = PLit (LitInt n)
anPat dt (P.PChar c) = PLit (LitChar c)
anPat dt (P.PStr s) = foldr (\c e -> pCons (PLit $ LitChar [c]) e) pNil s
anPat dt (P.AsPat i p) = PAs i (anPat dt p)
anPat dt P.Wildcard = PWildcard

anContext :: P.Context -> [Pred]
anContext = map anPred
    where anPred (id, t) = IsIn id (anType ([],[]) t)

anType :: TyconEnv -> P.Type -> Type
anType tce t = anTypeAp tce t []

anTypeAp :: TyconEnv -> P.Type -> [Type] -> Type
anTypeAp tce (P.TyAp t1 t2) ts = anTypeAp tce t1 (anType tce t2 : ts)
anTypeAp tce (P.TyTuple _) (t:ts) = error "bad application"
anTypeAp tce (P.TyTuple ts) [] = foldl TAp ttc ts'
    where ttc = TCon (tupTycon (length ts))
	  ts' = map (anType tce) ts
anTypeAp tce (P.TyVar i) ts = foldl TAp (TVar (Tyvar i Star)) ts
anTypeAp (tcs, syns) (P.TyCon i) ts =
    case (assoc i tcs, assoc i syns) of
    (Just tc, _) -> foldl TAp (TCon tc) ts
    (_, Just s@(Synonym i _ vs _)) ->
        let n = length vs
            (ts1, ts2) = splitAt n ts
        in if length ts1 == n then foldl TAp (TSynonym s ts1) ts2
           else error ("partial application of " ++ i)
    (Nothing, Nothing) -> error ("unknown type constructor " ++ i)


anDataDecls :: [Synonym] -> [(P.Context, String, [String], [P.Constr])]
               -> DataType
anDataDecls syns dts = (tcenv, constrs)
    where (userTycons, constrs_s) = unzip $ map (anDataDecl tcenv) dts
          tcenv = (preludeTycons ++ userTycons, preludeSynonyms ++ syns)
          constrs = concat (preludeConstrs : constrs_s)

anDataDecl :: TyconEnv -> (P.Context, String, [String], [P.Constr])
              -> (Tycon, [Const])
anDataDecl tce (ctx, con, vs, cs) = (tc, consts)
    where (qt, tc) = anDataLhs ctx con vs (length cs) arities
          consts = [Const { conName=i, conArity=a, conTag=tag,
                                     conTycon=tc, conScheme=sc}
              | (tag, (i, a, sc)) <- zip [1..] $ map (anConstr tce qt) cs]
          arities = [conArity c | c <- consts]

anDataLhs :: P.Context -> String -> [String] -> Int -> [Int]
          -> (Qual Type, Tycon)
anDataLhs ctx con vs n arities =
    (anContext ctx :=> foldl TAp (TCon tc) vs', tc)
    where k = foldr Kfun Star (replicate (length vs') Star)
	  tc = Tycon con k n arities
	  vs' = [TVar (Tyvar v Star) | v <- vs]

anConstr :: TyconEnv -> Qual Type -> P.Constr -> (Id, Int, Scheme)
anConstr tce (ps :=> dt) (i, ts) =
    case tv ts' \\ tv dt of
      [] -> (i, length ts', quantifyAll (ps :=> foldr fn dt ts'))
      (Tyvar i _ : _) -> error ("Undefined type variable " ++ i)
    where ts' = map (anType tce) ts

anTypeDecl :: DataType -> (String, [String], P.Type) -> Synonym
anTypeDecl (tce,_) (id, vs, t) = Synonym id k vs' t'
    where k   = foldr Kfun Star (replicate (length vs) Star)
          vs' = [Tyvar v Star | v <- vs]
          t'  = anType tce t
  -- FIXME: kind inference needed

-----------------------------------------------------------------------------
-- class declaration

anClassDecls :: DataType
                -> [(P.Context, String, P.Type, [P.Decl])]
                -> EnvTransformer
-- FIXME: check for cyclic inheritance
anClassDecls dt cds = foldl (<:>) idEnvTransformer (map (anClassDecl dt) cds)

anClassDecl :: DataType
               -> (P.Context, String, P.Type, [P.Decl])
               -> EnvTransformer
anClassDecl dt classDecl@(sup, id, v, decls) =
    T.addClass id (map fst sup) as <:> T.addImpls impls <:> T.addExpls expls
    where as = [i :>: sc | P.SigDecl (is, ctx, t) <- decls,
                           (i, sc) <- anSigDecl dt (is, (id, v) : ctx, t) ]
          impls = selectors classDecl as
          expls = anDefaultMethodDecl dt as decls

selectors :: (P.Context, String, P.Type, [P.Decl]) -> [Assump] -> [Impl]
selectors (sup, id, v, decls) as = supers ++ methods
    where supers = [superSelector s id k dictsize
                    | ((s, _), k) <- zip sup [0..]]
          methods = [tupleSelector m k dictsize
                     | (m :>: _, k) <- zip as [length sup..]]
          dictsize = length sup + length as

superSelector :: String -> String -> Int -> Int -> Impl
superSelector sup cls = tupleSelector (cls ++ ">>" ++ sup)

anDefaultMethodDecl :: DataType -> [Assump] -> [P.Decl] -> [Expl]
anDefaultMethodDecl dt as decls = map makeExpl impls
    where impls = anVarDecls dt [a | P.VarDecl a <- decls]
          makeExpl (id, alts) =
              case findAssump id as of
                Just sc -> (defaultMethodId id, sc, alts)
                Nothing -> error ("undeclared method: " ++ id)

-----------------------------------------------------------------------------
-- instance declaration

anInstDecls :: DataType -> [(P.Context, String, P.Type, [P.Decl])]
               -> EnvTransformer
anInstDecls dt ids = foldl (<:>) idEnvTransformer (map (anInstDecl dt) ids)

anInstDecl :: DataType -> (P.Context, String, P.Type, [P.Decl])
              -> EnvTransformer
anInstDecl dt@(tce,_) (ctx, id, t, decls) ce =
    (addinst <:> T.addExpls es <:> T.addImpls [dict]) ce
    where addinst = T.addInst ps p (Var dictid)
          ps      = anContext ctx
          t'      = anType tce t
          p       = IsIn id t'
          dataid  = makeDataId t'
          dictid  = id ++ dataid
          (bounded, es) = unzip (anInstMethodDecl ce dt ps p decls)
          dict    = makeDict ce ps p dictid bounded

makeDict :: ClassEnv -> [Pred] -> Pred -> Id -> [Id] -> Impl
makeDict ce ps pred dictid bounded = (dictid, alts)
    where alts = T.resolve ce [] [] ps [([], Rhs expr)]
          expr = tuple (supers ++ methods)
          supers = superDict ce pred
          methods = methodDict ce ps pred dictid bounded

superDict :: ClassEnv -> Pred -> [Expr]
superDict ce (IsIn cls t) = [ClassPH (IsIn sup t) | sup <- T.super ce cls]

methodDict :: ClassEnv -> [Pred] -> Pred -> Id -> [Id] -> [Expr]
methodDict ce ps (IsIn cls t) dictid bounded =
    [entry mth | mth :>: _ <- T.methods ce cls]
    where entry id = if elem id bounded
                     then foldl Ap (Var (methodId id t)) phs
                     else Ap (Var (defaultMethodId id))
                             (foldl Ap (Var dictid) phs)
          phs = map ClassPH ps

{- memo
Class Show a where
  show :: a -> String

instance (Show c, Show d) => Show (c, d) where
  show (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"

  showTup :: (Show c, Show d) => (c, d) -> String

show :: (Show a) => a -> String
-}
anInstMethodDecl :: ClassEnv -> DataType -> [Pred] -> Pred -> [P.Decl]
                    -> [(Id, Expl)]
anInstMethodDecl ce dt ps (IsIn cls t) decls =
    [(id, (methodId id t, sig id, alts)) | (id, alts) <- impls]
    where impls = anVarDecls dt [a | P.VarDecl a <- decls]
          sig :: Id -> Scheme
          sig id = quantifyAll ((ps' ++ apply sub ps) :=> apply sub t')
              where sub = [(v, t)]
                    (IsIn cls' (TVar v) : ps') :=> t' = T.runTI (T.freshInst sc)
                    Just sc = findAssump id (T.methods ce cls)

methodId :: Id -> Type -> Id
methodId mth t = mth ++ '#' : makeDataId t

defaultMethodId :: Id -> Id
defaultMethodId mth = mth ++ "#"

makeDataId :: Type -> Id
makeDataId t =
    case head (fromTAp t) of
      TCon tc -> if tyconName tc == "(,)"
                 then '(' : show (tyconKind tc) ++ ")"
                 else tyconName tc

lookupConst i cs = lookup i [(conName c, c) | c <- cs]
