module Syntax where
import Data.Char(chr, ord)
import Data.List(find, nub, union, intersect, (\\))
import SCC

type Id  = String
type Alt = ([Pat], Rhs)
type Expl = (Id, Scheme, [Alt])
type Impl   = (Id, [Alt])
type BindGroup  = ([Expl], [[Impl]])
type Program = [BindGroup]

data Kind  = Star | Kfun Kind Kind
             deriving (Eq, Show)

class Assoc a where
    assocKey :: a -> String
    assoc :: String -> [a] -> Maybe a
    assoc key [] = Nothing
    assoc key (x:xs) = if assocKey x == key then Just x else assoc key xs

-----------------------------------------------------------------------------
-- Type:		Types
-----------------------------------------------------------------------------

data Type  = TVar Tyvar
           | TCon Tycon
           | TAp Type Type
           | TGen Int
           | TSynonym Synonym [Type]
             deriving Eq

instance Show Type where
    showsPrec _ (TVar v) = shows v
    showsPrec _ (TCon c) = shows c
    showsPrec _ (TSynonym syn []) = shows syn
    showsPrec p (TSynonym syn ts) = showParen (p > 2) f
        where f = shows syn . (' ':) . g
              g = foldr1 (\l r -> l . (' ':) . r) (map (showsPrec 3) ts)
    showsPrec _ (TGen n) = (chr (ord 'a' + n) :)
    showsPrec p tap@(TAp _ _) =
	case t of
	  TCon tc | tyconName tc == "[]"
               -> ('[':) . showsPrec 0 t1 . (']':)
	      where [t1] = ts
	  TCon tc | tyconName tc == "(->)"
               -> showParen (p > 0) $
		  showsPrec 1 t1 . (" -> " ++) . showsPrec 0 t2
	      where [t1, t2] = ts
	  TCon tc | tyconName tc == "(,)"
               -> showParen True $
		  foldr1 (\f g -> f . (", " ++) . g)
                         (map (showsPrec 0) ts)
	  _    -> showParen (p > 2) $
		  foldr1 (\f g -> f . (' ':) . g)
                         (map (showsPrec 3) (t:ts))
	where (t:ts) = fromTAp tap

fromTAp :: Type -> [Type]
fromTAp (TAp t1 t2) = fromTAp t1 ++ [t2]
fromTAp t = [t]

data Tyvar = Tyvar Id Kind deriving Eq

instance Show Tyvar where
    show (Tyvar id _) = id

data Tycon = Tycon { tyconName::Id,
                     tyconKind::Kind,
                     tyconNumCon::Int,
                     tyconArities::[Int]
                   } deriving Eq

instance Show Tycon where
    show tc = tyconName tc

data Synonym = Synonym Id Kind [Tyvar] Type deriving Eq

instance Show Synonym where
    show (Synonym id _ _ _) = id

instance Assoc Tycon where
    assocKey tc = tyconName tc

instance Assoc Synonym where
    assocKey (Synonym i _ _ _) = i

unsynonym :: Synonym -> [Type] -> Type
unsynonym (Synonym _ _ vs t) ts = apply s t
    where s = zip vs ts

tChar    = TCon (Tycon "Char" Star 0 [])
tInt     = TCon (Tycon "Int" Star 0 [])
tBool    = TCon (Tycon "Bool" Star 2 [0,0])
tUnit    = TCon (Tycon "()" Star 1 [0])
tList    = TCon (Tycon "[]" (Kfun Star Star) 2 [2,0])
tArrow   = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)) 0 [])

tString    :: Type
tString     = list tChar

preludeTycons :: [Tycon]
preludeTycons = [Tycon "()" Star 1 [0],
		 Tycon "Char" Star 0 [],
		 Tycon "Int" Star 0 [],
		 Tycon "Bool" Star 2 [0,0],
		 Tycon "[]" (Kfun Star Star) 2 [2,0],
		 Tycon "(->)" (Kfun Star (Kfun Star Star)) 0 []
                ]

preludeSynonyms :: [Synonym]
preludeSynonyms = [Synonym "String" Star [] (list tChar)
                  ]

preludeConstrs :: [Const]
preludeConstrs = [Const { conName = i,
                          conArity = a,
                          conTag = tag,
                          conTycon = tycon,
                          conScheme = quantifyAll' t }
                      | (i, a, tag, TCon tycon, t) <- constrs]
    where a = TVar (Tyvar "a" Star)
          constrs = [("True", 0, 1, tBool, tBool),
		     ("False", 0, 2, tBool, tBool),
                     (":", 2, 1, tList, a `fn` list a `fn` list a),
		     ("[]", 0, 2, tList, list a)]

eTrue = Con con
    where Just con = find (\c -> conName c == "True") preludeConstrs
eFalse = Con con
    where Just con = find (\c -> conName c == "False") preludeConstrs
eCons = Con con
    where Just con = find (\c -> conName c == ":") preludeConstrs
eNil = Con con
    where Just con = find (\c -> conName c == "[]") preludeConstrs
pCons x y = PCon con [x, y]
    where Just con = find (\c -> conName c == ":") preludeConstrs
pNil = PCon con []
    where Just con = find (\c -> conName c == "[]") preludeConstrs

infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TAp (TAp tArrow a) b

list       :: Type -> Type
list t      = TAp tList t

pair       :: Type -> Type -> Type
pair a b    = TCon (tupTycon 2) `fn` a `fn` b

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind (Tyvar v k) = k
instance HasKind Tycon where
  kind tc = tyconKind tc
instance HasKind Synonym where
  kind (Synonym _ k _ _) = k
instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u)  = kind u
  kind (TAp t _) = case (kind t) of
                     (Kfun _ k) -> k
  kind (TSynonym syn ts) = kind (unsynonym syn ts)

type Subst  = [(Tyvar, Type)]

class Types t where
  apply :: Subst -> t -> t
  tv    :: t -> [Tyvar]

instance Types Type where
  apply s (TVar u)  = case lookup u s of
                       Just t  -> t
                       Nothing -> TVar u
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply s t         = t

  tv (TVar u)  = [u]
  tv (TAp l r) = tv l `union` tv r
  tv t         = []

instance Types a => Types [a] where
  apply s = map (apply s)
  tv      = nub . concat . map tv

-- Predicates
data Qual t = [Pred] :=> t
              deriving Eq

data Pred   = IsIn Id Type
              deriving Eq

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t)      = tv ps `union` tv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn i t)      = tv t

instance (Show t) => Show (Qual t) where
    showsPrec _ ([] :=> t) = shows t
    showsPrec _ (p :=> t) = showsContext . (" => " ++) . shows t
        where showsContext = showParen True $
                             foldr1 (\f g -> f . (", " ++) . g) (map shows p)

instance Show Pred where
    showsPrec _ (IsIn id t) = (id ++) . (' ':) . shows t

-- Type schemes
data Scheme = Forall [Kind] (Qual Type)
              deriving Eq

instance Show Scheme where
    showsPrec _ (Forall _ qt) = shows qt

instance Types Scheme where
  apply s (Forall ks t) = Forall ks (apply s t)
  tv (Forall ks t)      = tv t

quantify      :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where vs' = [ v | v <- tv qt, v `elem` vs ]
       ks  = map kind vs'
       s   = zip vs' (map TGen [0..])

quantifyAll :: Qual Type -> Scheme
quantifyAll t = quantify (tv t) t

quantifyAll' :: Type -> Scheme
quantifyAll' t = quantify (tv t) ([] :=> t)

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)

-- Assumptions
data Assump = Id :>: Scheme

instance Show Assump where
    show (i :>: sc) = show i ++ " :: " ++ show sc

instance Types Assump where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv (i :>: sc)      = tv sc

findAssump :: Monad m => Id -> [Assump] -> m Scheme
findAssump id [] = fail ("unbound identifier: " ++ id)
findAssump id ((i:>:sc):as) = if i == id then return sc else findAssump id as

-- Literals
data Literal = LitInt  Int
             | LitChar String
             | LitStr  String
               deriving Eq

instance Show Literal where
    show (LitInt n) = show n
    show (LitChar c) = c
    show (LitStr s) = s

-- Patterns
data Pat  = PVar Id
          | PWildcard
          | PAs  Id Pat
          | PLit Literal
          | PCon Const [Pat]

data Expr = Var     Id
	  | Lit     Literal
	  | Con     Const
	  | Ap      Expr Expr
	  | Let     BindGroup Expr
	  | Case    Expr [(Pat, Rhs)]
	  | Lambda  Alt
	  | ESign   Expr Scheme
          | RecPH   Id
          | ClassPH Pred

data Rhs = Rhs Expr
         | Where BindGroup Rhs
         | Guarded [(Expr, Expr)]

data Const = Const { conName::Id,
                     conArity::Int,
                     conTag::Int,
                     conTycon::Tycon,
                     conScheme::Scheme }

instance Eq Const where
    c1 == c2 = conName c1 == conName c2

ap :: Expr -> [Expr] -> Expr
ap = foldl Ap

bindings :: BindGroup -> [Impl]
bindings (es, iss) = [(i, as) | (i, _, as) <- es] ++ concat iss

class HasVar t where
    freeVars :: t -> [Id]

instance HasVar Expr where
    freeVars (Var i) = [i]
    freeVars (Ap e1 e2) = freeVars e1 `union` freeVars e2
    freeVars (Let bg e) = fvBindGroup bg `union`
                         (freeVars e \\ map fst (bindings bg))
    freeVars (Case e pses) = foldr union fve fvas
        where fve = freeVars e
	      fvas = [freeVars e' \\ patVars p | (p, e') <- pses]
    freeVars (Lambda a) = fvAlt a
    freeVars (ESign e _) = freeVars e
    freeVars _ = []

instance HasVar Rhs where
    freeVars (Rhs e) = freeVars e
    freeVars (Where bg rhs) =
        fvBindGroup bg `union` (freeVars rhs \\ map fst (bindings bg))
    freeVars (Guarded pairs) =
        foldr union [] [freeVars e `union` freeVars e' | (e, e') <- pairs]

fvBindGroup :: BindGroup -> [Id]
fvBindGroup bg = fvAlts (concat altss) \\ is
    where (is, altss) = unzip (bindings bg)

fvAlts :: [Alt] -> [Id]
fvAlts alts = foldl1 union (map fvAlt alts)
fvAlt :: Alt -> [Id]
fvAlt (ps, rhs) = freeVars rhs \\ concat (map patVars ps)

patVars :: Pat -> [Id]
patVars (PVar i) = [i]
patVars (PAs i p) = i : patVars p
patVars (PCon _ ps) = concat (map patVars ps)
patVars _ = []

tupcon :: Int -> Const
tupcon n = Const "(,)" n 1 tycon sc
    where tycon = tupTycon n
          tuptype = foldl TAp (TCon tycon) tvars
{-
	  tvars = [TVar (Tyvar ('v' : show i) Star) | i <- [0..n-1]]
          scheme = quantifyAll (foldr fn tuptype tvars)
-}
	  tvars = [TGen i | i <- [0..n-1]]
          sc = Forall (replicate n Star) ([] :=> foldr fn tuptype tvars)


tupTycon :: Int -> Tycon
tupTycon n = Tycon "(,)" (foldr Kfun Star (replicate n Star)) 1 [0]

tuple :: [Expr] -> Expr
tuple es = foldl Ap (Con $ tupcon $ length es) es

tupleSelector :: String -> Int -> Int -> Impl
tupleSelector id k n = (id, [([pat], Rhs expr)])
    where pat = PCon (tupcon n) [PVar ('e' : show i) | i <- [0..n-1]]
          expr = Var ('e' : show k)

-- type class

type Class    = ([Id], [Inst], [Assump])
type Inst     = (Qual Pred, Expr)

data ClassEnv = ClassEnv { classes  :: Id -> Maybe Class,
                           defaults :: [Type],
                           impls    :: [Impl],
                           expls    :: [Expl],
                           assumps  :: [Assump] }

type EnvTransformer = ClassEnv -> Maybe ClassEnv
idEnvTransformer :: EnvTransformer
idEnvTransformer ce = Just ce

infixr 5 <:>
(<:>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do ce' <- f ce
                  g ce'

-- SKI expression
data SKI = SAp  SKI SKI
	 | SLit Literal
	 | SVar Id
	 | SCon Int Int

sap :: SKI -> [SKI] -> SKI
sap = foldl SAp

instance Show SKI where
    show e = showsPrec 1 e ""
    showsPrec _ (SVar i)    = (i++)
    showsPrec _ (SLit l)    = shows l
    showsPrec _ (SCon k n)  = ('@':) . shows k . ('_':) . shows n
    showsPrec _ (SAp e1 e2) = ('`':) . shows e1 . shows e2
--    showsPrec p (SAp e1 e2) = showParen (p > 0) $
--			        showsPrec 0 e1 . (' ':) . showsPrec 1 e2

dependency :: [Impl] -> [[Impl]]
dependency bs = (map . map) (\v -> (v, lookup' v bs)) (reverse vss)
    where vs = map fst bs
	  vss = scc [(v, fvAlts alts `intersect` vs) | (v, alts) <- bs]
	  lookup' key xs = case lookup key xs of
			   Just x -> x
			   Nothing -> error "cannot occur"
