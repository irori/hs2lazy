module PatComp (compilePatternMatch, patBindings) where
import Data.List
import Control.Monad hiding (ap)
import Control.Monad.State hiding (ap)
import Syntax
import PPrint ()

type PatComp = State Int

compilePatternMatch :: Program -> Program
compilePatternMatch pgm = evalState (pcProgram pgm) 0

pcProgram :: Program -> PatComp Program
pcProgram = mapM pcBindGroup

pcBindGroup :: BindGroup -> PatComp BindGroup
pcBindGroup (es, iss) =
    do es' <- mapM pcExpl es
       iss' <- mapM (mapM pcImpl) iss
       return (es', iss')

pcExpl :: Expl -> PatComp Expl
pcExpl (i, sc, alts) = do { alt <- pcAlts alts; return (i, sc, [alt]) }

pcImpl :: Impl -> PatComp Impl
pcImpl (i, alts) = do { alt <- pcAlts alts; return (i, [alt]) }

pcAlts :: [Alt] -> PatComp Alt
pcAlts [(ps, Rhs e)]
    | all isPVar ps = do { e' <- pcExpr e; return (ps, Rhs e') }
    where isPVar (PVar _) = True
	  isPVar _ = False
pcAlts qs = do us <- newVars n
               rhs <- match us qs matchError
               return (map PVar us, Rhs rhs)
    where n = length $ fst $ head $ qs

pcRhs :: Rhs -> Expr -> PatComp Expr
pcRhs (Rhs e) _ = pcExpr e
pcRhs (Where bg rhs) def = liftM (Let bg) (pcRhs rhs def)
pcRhs (Guarded gds) def = do gds' <- mapM pcGuard gds
                             return $ foldr makeIf def gds'

pcGuard :: (Expr, Expr) -> PatComp (Expr, Expr)
pcGuard (e1, e2) = do e1' <- pcExpr e1
                      e2' <- pcExpr e2
                      return (e1', e2')

pcExpr :: Expr -> PatComp Expr
pcExpr (Ap e1 e2) = do e1' <- pcExpr e1
                       e2' <- pcExpr e2
                       return (Ap e1' e2')
pcExpr (Let bg e) = do bg' <- pcBindGroup bg
                       e'  <- pcExpr e
                       return (Let bg' e')
pcExpr (Lambda a) = liftM Lambda (pcAlts [a])
pcExpr c@(Case e pes) =
    do e' <- pcExpr e
       case e' of
         (Var v) -> match [v] qs matchError
         e''     -> do v <- newVar
                       m <- match [v] qs matchError
                       return $ Let (bind1 v e'') m
    where qs = [([p], rhs) | (p, rhs) <- pes]
pcExpr (ESign e sc) = do { e' <- pcExpr e; return $ ESign e' sc }
pcExpr c = return c

matchError :: Expr
matchError = Ap (Var "error") (Lit $ LitStr $ "Non-exhaustive patterns")

patBindings :: Expr -> Pat -> [Impl]
patBindings v (PVar i) = [(i, [([], Rhs v)])]
patBindings _ PWildcard = []
patBindings v (PAs i p) = (i, [([], Rhs v)]) : patBindings v p
patBindings v (PLit _) = []
patBindings v (PCon con pats)
    = concat [patBindings (makeSel con n v) p | (p, n) <- zip pats [1..]]

makeSel :: Const -> Int -> Expr -> Expr
--makeSel con i e = ap (Var "SEL") [Lit (LitInt i), e]
makeSel con i e = expr
    where vs = ["@@"++show v | v <- [1..(conArity con)]]
          body = Rhs $ Var $ vs !! (i-1)
          receiver = Lambda ([PVar v | v <- vs], body)
          expr = ap e [if i == conTag con then receiver else eError
                       | i <- [1..(tyconNumCon $ conTycon con)]]
          eError = Ap (Var "error") (Lit $ LitStr $ "!?")

type Equation = Alt

isVar :: Equation -> Bool
isVar (p:_, _) = test p
    where test (PVar _) = True
          test PWildcard = True
          test (PAs _ p) = test p
          test (PLit _) = False
          test (PCon _ _) = False

partitionEqns :: Eq b => (a -> b) -> [a] -> [[a]]
partitionEqns f [] = []
partitionEqns f [x] = [[x]]
partitionEqns f (x:xs@(x':_)) | f x == f x' = tack x (partitionEqns f xs)
                          | otherwise   = [x] : partitionEqns f xs
tack x xss = (x : head xss) : tail xss

match :: [Id] -> [Equation] -> Expr -> PatComp Expr
match [] qs def = foldrM pcRhs def (map snd qs)
match us qs def =
    foldrM (matchVarCon us) def (partitionEqns isVar qs)

matchVarCon :: [Id] -> [Equation] -> Expr -> PatComp Expr
matchVarCon us@(u:_) qs def =
    case head $ fst $ head qs' of
      PLit _   -> bindDefault (matchLit us qs') def
      PCon _ _ -> bindDefault (matchCon us qs') def
      _        -> matchVar us qs' def
    where qs' = map sub qs
          sub (PAs v p : ps, rhs) = sub (p:ps, Where (bind1 v (Var u)) rhs)
          sub (ps, rhs) = (ps, rhs)

matchVar :: [Id] -> [Equation] -> Expr -> PatComp Expr
matchVar (u:us) qs def = match us (map sub qs) def
    where sub (PVar v : ps, rhs) = (ps, Where (bind1 v (Var u)) rhs)
          sub (PWildcard : ps, rhs) = (ps, rhs)

bindDefault :: (Expr -> PatComp Expr) -> Expr -> PatComp Expr
bindDefault f def
    | simple def = f def
    | otherwise  = do v <- newVar
                      e <- f (Var v)
                      return $ Let (bind1 v def) e
    where simple _ = True
{- これでは効率悪い -- 式のサイズで分けるとか?
    where simple (Var _) = True
          simple (Lit _) = True
          simple (Con _) = True
          simple _       = False
-}

matchLit :: [Id] -> [Equation] -> Expr -> PatComp Expr
matchLit us qs def =
    do clauses <- mapM (matchLitClause us def) (groupLit qs)
       return $ foldr makeIf def clauses

matchLitClause :: [Id] -> Expr -> (Literal, [Equation]) -> PatComp (Expr, Expr)
matchLitClause (u:us) def (lit, qs) =
    do e <- match us [(ps,rhs) | (_:ps,rhs)<-qs] def
       return (ap (Var "&eq") [Var u, Lit lit], e)

groupLit :: [Equation] -> [(Literal, [Equation])]
groupLit [] = []
groupLit qs@((PLit l:_,_):_) = (l, qs') : groupLit qs''
    where (qs', qs'') = partition (\(PLit l':_,_) -> l == l') qs

matchCon :: [Id] -> [Equation] -> Expr -> PatComp Expr
matchCon us qs def
    | isCovered grps =
        do clauses <- mapM (matchConClause us def) (init grps)
           lastClause <- matchConLastClause us def (last grps)
           return $ foldr makeIf lastClause clauses
    | otherwise =
        do clauses <- mapM (matchConClause us def) grps
           return $ foldr makeIf def clauses
    where grps = groupCon qs

{- matchConClause [a,b] def ((:), [([: x xs, y], rhs)])
   => (TAGEQ (:) a, let v1 = SEL 1 a, v2 = SEL 2 a in
                      (match [v1,v2,b] [([x,xs,y],rhs)] def)
   => (TAGEQ (:) a, a (\v1 v2 -> (match [v1,v2,b] [([x,xs,y],rhs)] def)) error)
-}
matchConClause :: [Id] -> Expr -> (Const, [Equation]) -> PatComp (Expr, Expr)
matchConClause (u:us) def (con, qs) =
    do us' <- newVars k'
       body <- match (us'++us)
                     [(ps'++ps, rhs) | (PCon c ps':ps, rhs) <- qs] def
       let receiver = Lambda ([PVar v | v <- us'], Rhs body)
           expr = ap (Var u) [if i == conTag con then receiver else eError
                                 | i <- [1..(tyconNumCon $ conTycon con)]]
        in return (cond, expr)
    where cond = makeTagEq con (Var u)
          k' = conArity con
          eError = Ap (Var "error") (Lit $ LitStr $ "!?")

matchConLastClause :: [Id] -> Expr -> (Const, [Equation]) -> PatComp Expr
matchConLastClause us def grp = do (_, e) <- matchConClause us def grp
                                   return e

groupCon :: [Equation] -> [(Const, [Equation])]
groupCon [] = []
groupCon qs@((PCon c _:_,_):_) = (c, qs') : groupCon qs''
    where (qs', qs'') = partition (\(PCon c' _:_,_) -> c == c') qs

isCovered :: [(Const, [Equation])] -> Bool
isCovered grps = n == 0 || length grps == n
    where n = tyconNumCon $ conTycon $ fst $ head grps

newVars :: Int -> PatComp [Id]
newVars k = do n <- get
               put (n+k)
               return ['@':show (n+i) | i <- [1..k]]

newVar :: PatComp Id
newVar = do n <- get
            put (n+1)
            return $ '@':show (n+1)

makeIf :: (Expr, Expr) -> Expr -> Expr
makeIf (c, e) e' = ap (Var "IF") [c, e, e']

{- makeTagEq (:) e => e (\x -> \xs -> True) False
-}
makeTagEq :: Const -> Expr -> Expr
--makeTagEq con e = ap (Var "TAGEQ") [Con con, e]
makeTagEq con e = ap e es
    where arities = tyconArities (conTycon con)
          es = [test a (b == conTag con) | (a, b) <- zip arities [1..]]
          test arity b =
              Lambda ([PVar ('_':show n) | n <- [1..arity]],
                      Rhs $ if b then eTrue else eFalse)

bind1 :: Id -> Expr -> BindGroup
bind1 v e = ([], [[(v, [([], Rhs e)])]])

foldrM :: Monad m => (b -> a -> m a) -> a -> [b] -> m a
foldrM f a xs = foldM (flip f) a (reverse xs)
