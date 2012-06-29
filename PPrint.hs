module PPrint where
import Syntax
--import IOExts (trace)

--traceProgram :: Program -> Program
--traceProgram p = trace (showProgram p) p

showProgram :: Program -> String
showProgram p = join ";\n" (map ssBindGroup p) ""

join :: String -> [ShowS] -> ShowS
join _ [] = id
join sep ss = foldr1 (\l r -> l . (sep++) . r) ss

ssBindGroup :: BindGroup -> ShowS
ssBindGroup (es, iss) = join ";\n" (showses ++ showsis)
    where showses = concatMap ssExpl es
	  showsis = concatMap ssImpl (concat iss)
	  ssExpl (i, sc, alts) = ssSig i sc : map (ssDef i) alts
	  ssImpl (i, alts)     = map (ssDef i) alts

ssSig :: Id -> Scheme -> ShowS
ssSig i sc = (i++) . (" :: "++) . shows sc

ssDef :: Id -> Alt -> ShowS
ssDef i (ps, rhs) = lhs . ssRhs " = " rhs
    where lhs = join " " ((i++) : map (showsPrec 1) ps)

ssRhs :: String -> Rhs -> ShowS
ssRhs sep (Rhs e) = (sep++) . showsPrec 0 e
ssRhs sep (Guarded gds) = join "\n\t" gds'
    where gds' = [(" | "++) . showsPrec 0 cond . (sep++) . showsPrec 0 e
                  | (cond, e) <- gds]
ssRhs sep rhs@(Where _ _) =
    ssRhs sep rhs' . ("\n\twhere {"++) . ssBindGroup bg . ('}':)
    where (bg, rhs') = collectBg rhs

collectBg :: Rhs -> (BindGroup, Rhs)
collectBg (Where (es, iss) rhs) = ((es++es', iss++iss'), rhs')
    where ((es', iss'), rhs') = collectBg rhs
collectBg rhs = (([],[]), rhs)

instance Show Expr where
    showsPrec _ (Var i) = (i++)
    showsPrec _ (Lit l) = shows l
    showsPrec _ (Con con) = (conName con ++)
    showsPrec p (Ap e1 e2) = showParen (p > 2) $
			     showsPrec 2 e1 . (' ':) . showsPrec 3 e2
    showsPrec p (Let bg e) = showParen (p > 0) $
	("let {"++) . ssBindGroup bg . ("} in "++) . showsPrec 0 e
    showsPrec p (Case e pes) = showParen (p > 0) $
	("case "++) . showsPrec 0 e . (" of {"++) . (join ";\n\t" alts) . ('}':)
	where alts = [showsPrec 0 p . ssRhs " -> " rhs | (p, rhs) <- pes]
    showsPrec p (Lambda (ps, Rhs e)) = showParen (p > 0) $
	("\\"++) . join " " (map (showsPrec 1) ps) . ("-> "++) . showsPrec 0 e
    showsPrec p (ESign e sc) = showParen True $
	showsPrec 0 e . (" :: "++) . shows sc
    showsPrec p (ClassPH pred) = ('<':) . shows pred . ('>':)
    showsPrec p (RecPH i) = ('<':) . (i++) . ('>':)

instance Show Pat where
    showsPrec _ (PVar i) = (i++)
    showsPrec _ PWildcard = ('_':)
    showsPrec p (PAs i pat) = showParen (p > 1) $
			      (i++) . ('@':) . showsPrec 1 pat
    showsPrec _ (PLit l) = shows l
    showsPrec p (PCon con []) = (conName con ++)
    showsPrec p (PCon con ps) = showParen (p > 0) $
				join " " ((conName con++) : map (showsPrec 1) ps)

instance Show Rhs where
    showsPrec _ = ssRhs " -> "
