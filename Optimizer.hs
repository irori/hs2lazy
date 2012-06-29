module Optimizer where
import Syntax

optimizeExpr :: Expr -> Expr
optimizeExpr = optExpr

optExpr :: Expr -> Expr
optExpr e@(Var _) = e
optExpr e@(Lit _) = e
optExpr e@(Con _) = e
optExpr (Ap e1 e2) = Ap (optExpr e1) (optExpr e2)
optExpr (Let bg e) = optLet bg e
optExpr (Lambda (vs, Rhs e)) = Lambda (vs, Rhs (optExpr e))
optExpr (ESign e sc) = optExpr e

optBindGroup :: [Impl] -> BindGroup
optBindGroup is = ([], [is'])
    where is' = [(i, [(vs, Rhs (optExpr e))]) | (i, [(vs, Rhs e)]) <- is]

optLet :: BindGroup -> Expr -> Expr
optLet bg e = case bindings bg of
                [(v, [([], Rhs e')])] | simple e' -> optExpr (substVar e' v e)
                is -> Let (optBindGroup is) (optExpr e)
    where simple (Var _) = True
          simple (Lit _) = True
          simple (Con _) = True
          simple _       = False

substVar :: Expr -> Id -> Expr -> Expr
substVar e' v e@(Var i) | i == v = e'
                        | otherwise = e
substVar e' v e@(Lit _) = e
substVar e' v e@(Con _) = e
substVar e' v (Ap e1 e2) = Ap (substVar e' v e1) (substVar e' v e2)
substVar e' v (Let bg e)
    | v `elem` map fst (bindings bg) = Let bg e
    | otherwise = Let (subVarBindGroup e' v bg) (substVar e' v e)
substVar e' v (Case e gds) =
    Case (substVar e' v e) [(p, if v `elem` patVars p
                                then rhs else subVarRhs e' v rhs)
                           | (p, rhs) <- gds]
substVar e' v (Lambda alt) = Lambda (subVarAlt e' v alt)
substVar e' v (ESign e sc) = ESign (substVar e' v e) sc

subVarBindGroup :: Expr -> Id -> BindGroup -> BindGroup
subVarBindGroup e' v (es, iss) = (es', iss')
    where es' = [(i, sc, subVarAlts e' v alts) | (i, sc, alts) <- es]
          iss' = map (\is -> [(i, subVarAlts e' v alts) | (i, alts) <- is]) iss

subVarAlts :: Expr -> Id -> [Alt] -> [Alt]
subVarAlts e' v = map (subVarAlt e' v)
subVarAlt :: Expr -> Id -> Alt -> Alt
subVarAlt e' v (ps, rhs) | any (elem v) (map patVars ps) = (ps, rhs)
                         | otherwise = (ps, subVarRhs e' v rhs)

subVarRhs :: Expr -> Id -> Rhs -> Rhs
subVarRhs e' v (Rhs e) = Rhs (substVar e' v e)
subVarRhs e' v (Guarded gds) = Guarded [(substVar e' v c, substVar e' v e)
                                        | (c, e) <- gds]
subVarRhs e' v (Where bg rhs)
    | v `elem` map fst (bindings bg) = Where bg rhs
    | otherwise = Where (subVarBindGroup e' v bg) (subVarRhs e' v rhs)
