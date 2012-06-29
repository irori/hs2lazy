module Compiler where
import Syntax
import PatComp (compilePatternMatch)
import PPrint () -- for (Show Expr)

programToExpr :: Program -> Expr
programToExpr bgs = foldr Let (mainExpr (last bgs)) bgs'
    where bgs' = regroup (init bgs)
	  mainExpr :: BindGroup -> Expr
	  mainExpr bg = case bindings bg of
			[("@main", [([], Rhs e)])] -> e
			_ -> error "Illegal program entry point"

regroup :: [BindGroup] -> [BindGroup]
regroup bgs = [([], [is]) | is <- iss]
    where iss = dependency (concat (map bindings bgs))

expandCon :: Expr -> Expr
expandCon e@(Var _) = e
expandCon e@(Lit _) = e
expandCon (Ap e1 e2) = Ap (expandCon e1) (expandCon e2)
expandCon (Let bg e) = Let (expandConBG bg) (expandCon e)
expandCon (Lambda (vs, Rhs e)) = Lambda (vs, Rhs (expandCon e))
expandCon (ESign e sc) = (ESign (expandCon e) sc)
expandCon (Con con) = Lambda ([PVar v | v <- as++fs], Rhs body)
    where as = ["@a" ++ show i | i <- [1..conArity con]]
          fs = ["@f" ++ show i | i <- [1..(tyconNumCon $ conTycon con)]]
          body = ap (Var $ fs !! (tag - 1)) [Var v | v <- as]
          tag = if conTag con < 1 then error ("bad tag " ++ conName con) else conTag con

expandConBG :: BindGroup -> BindGroup
expandConBG (es, iss) = (es', map expandConImpls iss)
    where es' = [(i, sc, map expandConAlt alts) | (i, sc, alts) <- es]
          expandConImpls is = [(i, map expandConAlt alts) | (i, alts) <- is]
          expandConAlt (ps, rhs) = (ps, expandConRhs rhs)
          expandConRhs (Rhs e) = Rhs (expandCon e)
          expandConRhs (Where bg rhs) = Where (expandConBG bg)
                                              (expandConRhs rhs)
          expandConRhs (Guarded pairs) = Guarded [(expandCon c, expandCon e)
                                                  | (c, e) <- pairs]

skiCompile :: Expr -> SKI
skiCompile = compileExpr

compileExpr :: Expr -> SKI
compileExpr (Ap e1 e2) = compileExpr e1 `SAp` compileExpr e2
compileExpr (Let bg e)
    = case map compileDef (bindings bg) of
      [(i, v)] -> case (abstract i e') of
		  SVar "K" `SAp` _ -> e'
		  e''              -> e'' `SAp` removeSelfRec i v
      defs     -> compileMultipleDefs e' defs
    where e' = compileExpr e
compileExpr (Lambda a) = compileAlt a
compileExpr (Var i) = SVar i
compileExpr (Lit l) = SLit l
compileExpr (Con con) = SCon (conTag con) (conArity con)
compileExpr e = error ("compileExpr: " ++ show e)

compileDef :: (Id, [Alt]) -> (Id, SKI)
compileDef (i, [a]) = (i, compileAlt a)

removeSelfRec :: Id -> SKI -> SKI
removeSelfRec i e
    | refers i e = SVar "Y" `SAp` abstract i e
    | otherwise  = e

compileMultipleDefs :: SKI -> [(Id, SKI)] -> SKI
compileMultipleDefs e defs
    | not $ any (flip refers e) (map fst defs) = e
    | otherwise = SAp lhs rhs
    where (is, vals) = unzip defs
	  lhs = uAbs is e
	  rhs = SVar "Y" `SAp` uAbs is (mklist vals)
	  mklist []     = SVar "nil"
	  mklist (x:xs) = SVar "cons" `SAp` x `SAp` mklist xs

uAbs :: [Id] -> SKI -> SKI
uAbs [] e     = SVar "K" `SAp` e
uAbs (i:is) e = SVar "U" `SAp` abstract i (uAbs is e)

compileAlt :: Alt -> SKI
compileAlt ([], Rhs e) = compileExpr e
compileAlt (PVar v : as, e) = abstract v (compileAlt (as, e))
compileAlt (p:ps, e) = error ("malformed pattern " ++ show p)

abstract :: Id -> SKI -> SKI
abstract i v@(SVar i') | i == i' = SVar "I"
		       | otherwise = SVar "K" `SAp` v
abstract i (SAp e1 e2)
    | refers i e1 || refers i e2 =
        sap (SVar "S") [abstract i e1, abstract i e2]
    | otherwise =
        SAp (SVar "K") (SAp e1 e2)
abstract i l@(SLit _) = SVar "K" `SAp` l
abstract i c@(SCon _ _) = SVar "K" `SAp` c

refers :: Id -> SKI -> Bool
refers i (SVar i')   = i == i'
refers i (SAp e1 e2) = refers i e1 || refers i e2
refers i (SLit _)    = False
refers i (SCon _ _)  = False
