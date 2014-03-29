module Parser where
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (satisfy)
import Text.ParserCombinators.Parsec.Expr
import Lexer

data TopDecl = Decl Decl
	     | DataDecl (Context, String, [String], [Constr])
	     | TypeDecl (String, [String], Type)
             | ClassDecl (Context, String, Type, [Decl])
             | InstDecl (Context, String, Type, [Decl])
             | ImpDecl
	       deriving Show

type Constr = (String, [Type])

type Context = [(String, Type)] -- Eq a : [("Eq", TyVar "a")]

data Decl = VarDecl (String, [Pat], Rhs)
          | PatBind (Pat, Rhs)
	  | SigDecl ([String], Context, Type)
	    deriving Show

data Rhs = Rhs Expr [Decl]
         | Guarded [(Expr, Expr)] [Decl]
           deriving Show

data Type = TyTuple [Type]
	  | TyVar String
	  | TyCon String
	  | TyAp Type Type
	    deriving Show

data Expr = Var String
	  | Con String
	  | Tuple [Expr]
	  | LitInt Int
	  | LitChar String
	  | LitStr String
	  | Ap Expr Expr
	  | Let [Decl] Expr
	  | Lambda [Pat] Expr
	  | ESign Expr Context Type
	  | Case Expr [(Pat, Rhs)]
	    deriving Show

data Pat = PVar String
         | PCon String [Pat]
         | PTuple [Pat]
         | PInt Int
	 | PChar String
	 | PStr String
	 | AsPat String Pat
	 | Wildcard
           deriving Show

data Fixity = InfixN Int | InfixL Int | InfixR Int deriving Eq

type HsParser a = GenParser (Token, SourcePos) () a

-----------------------------------------------------------------------------
-- declarations

program :: HsParser [TopDecl]
program = braceList topdecl

topdecl :: HsParser TopDecl
topdecl = datadecl <|> typedecl <|> classdecl <|> instdecl <|> liftM Decl decl <|> impdecl

impdecl :: HsParser TopDecl
impdecl = do tokenImport
             _ <- sepBy1 conid varsym
             skipMany tokenHiding
             skipMany (parenList var)
             return ImpDecl

datadecl :: HsParser TopDecl
datadecl = do tokenData
              (ctx, (c, vs)) <- context_simpletype
              tokenEq
              cs <- constrs
              return (DataDecl (ctx, c, vs, cs))

typedecl :: HsParser TopDecl
typedecl = do tokenType
              (c, vs) <- simpletype
              tokenEq
              t <- type_
              return (TypeDecl (c, vs, t))

classdecl :: HsParser TopDecl
classdecl = do tokenClass
               (ctx, (id, t)) <- qualType scontext test
               decls <- whereList cdecl
               return (ClassDecl (ctx, id, t, decls))
    where test (TyAp (TyCon id) t@(TyVar _)) = return (id, t)
          test x = unexpected (show x)

instdecl :: HsParser TopDecl
instdecl = do tokenInstance
              (ctx, (id, t)) <- qualType scontext pred
              decls <- whereList idecl
              return (InstDecl (ctx, id, t, decls))
    where pred (TyAp (TyCon id) t) = do { t' <- test inst t; return (id, t) }
          pred t = unexpected (show t)
          inst (TyCon _) = True
          inst (TyAp t (TyVar _)) = inst t
          inst (TyTuple []) = True
          inst (TyTuple (TyVar _ : ts)) = inst (TyTuple ts)

decls :: HsParser [Decl]
decls = braceList decl

decl :: HsParser Decl
decl = gendecl
   <|> do (id, pats) <- try funlhs
          r <- rhs
          return (VarDecl (id, pats, r))
   <|> do pat <- opPat
          r <- rhs
          return (PatBind (pat, r))

cdecl :: HsParser Decl
cdecl = gendecl <|> idecl

idecl :: HsParser Decl
idecl = do (id, pats) <- funlhs
           r <- rhs
           return (VarDecl (id, pats, r))

gendecl :: HsParser Decl
gendecl = do vs <- try varsCoco
             (ctx, t) <- context_type
             return (SigDecl (vs, ctx, t))
   -- <|> fixity

whereList :: HsParser a -> HsParser [a]
whereList p = option [] (do { tokenWhere; braceList p })

varsCoco :: HsParser [String]
varsCoco = do { vs <- vars; tokenCoco; return vs }

rhs :: HsParser Rhs
rhs = do tokenEq
         e <- expr
         ds <- whereDecls
         return (Rhs e ds)
  <|> do gds <- many1 (guard tokenEq)
         ds <- whereDecls
         return (Guarded gds ds)

constrs :: HsParser [Constr]
constrs = sepBy1 constr tokenBar

constr :: HsParser Constr
constr = do id <- con
            ts <- many atype
            return (id, ts)
  -- <|> infix conop <|> fielddecl

-----------------------------------------------------------------------------
-- types

context_type :: HsParser (Context, Type)
context_type = qualType context return

context_simpletype :: HsParser (Context, (String, [String]))
context_simpletype = qualType context toSimpletype

qualType :: (Type -> HsParser Context) -> (Type -> HsParser a)
         -> HsParser (Context, a)
qualType toContext toType =
  do t1 <- type_
     do tokenImply
        ctx <- toContext t1
        t2  <- type_
        t   <- toType t2
        return (ctx, t)
      <|> do { t <- toType t1; return ([], t) }


context :: Type -> HsParser Context
context (TyTuple ts) = mapM class_ ts
context t = mapM class_ [t]

scontext :: Type -> HsParser Context
scontext (TyTuple ts) = mapM simpleclass ts
scontext t = mapM simpleclass [t]

class_ :: Type -> HsParser (String, Type)
class_ (TyAp (TyCon cls) t) = if test t then return (cls, t)
                                        else unexpected (show t)
    where test (TyVar _) = True
          test (TyAp l _) = test l
          test _ = False
class_ t = unexpected (show t)

simpleclass :: Type -> HsParser (String, Type)
simpleclass (TyAp (TyCon cls) t@(TyVar var)) = return (cls, t)
simpleclass t = unexpected (show t)

toSimpletype :: Type -> HsParser (String, [String])
toSimpletype (TyCon con) = return (con, [])
toSimpletype (TyAp l (TyVar v)) = do (c, vs) <- toSimpletype l
                                     return (c, vs++[v])
toSimpletype t = unexpected (show t)

simpletype :: HsParser (String, [String])
simpletype = do c <- conid
                vs <- many varid
                return (c, vs)

type_ :: HsParser Type
type_ = chainr1 btype ap
    where ap = do { tokenRArrow; return arrow }

btype :: HsParser Type
btype = chainl1 atype (return TyAp)

atype :: HsParser Type
atype = do { t <- brackets type_; return (TyAp (TyCon "[]") t) }
    <|> do ts <- parenList type_
           case ts of
             [t] -> return t
             _   -> return (TyTuple ts)
    <|> qtycon
    <|> tyvar

qtycon :: HsParser Type
qtycon = do { i <- conid; return (TyCon i) }

tyvar :: HsParser Type
tyvar = do { i <- varid; return (TyVar i) }

-----------------------------------------------------------------------------
-- expressions

expr :: HsParser Expr
expr = do e <- opExp
          option e $ do tokenCoco
                        (ctx, t) <- context_type
                        return (ESign e ctx t)

opExp :: HsParser Expr
opExp = buildExpressionParser (opTable qop) exp10

opTable op = [[Infix (op (InfixN n)) AssocNone,
               Infix (op (InfixL n)) AssocLeft,
               Infix (op (InfixR n)) AssocRight] | n <- reverse [0..9]]

qop :: Fixity -> HsParser (Expr -> Expr -> Expr)
qop f = qvarop f <|> qconop f

qvarop :: Fixity -> HsParser (Expr -> Expr -> Expr)
qvarop f = do { op <- varsymF f; return (ap2 (Var op)) }

qconop :: Fixity -> HsParser (Expr -> Expr -> Expr)
qconop f = do { op <- consymF f; return (ap2 (Con op)) }

exp10 :: HsParser Expr
exp10 = do tokenLambda
           ps <- many1 apat
           tokenRArrow
           e <- expr
           return (Lambda ps e)
    <|> do { tokenLet; ds <- decls; tokenIn; e <- expr; return (Let ds e) }
    <|> do   tokenIf; e1 <- expr
             tokenThen; e2 <- expr
             tokenElse; e3 <- expr
             return (bltinApp "IF" [e1, e2, e3])
    <|> do { tokenCase; e <- expr; tokenOf; a <- alts; return (Case e a) }
    <|> do { tokenDo; fail "not implemented" }
    <|> fexp

fexp :: HsParser Expr
fexp = chainl1 aexp (return Ap)

aexp :: HsParser Expr
aexp = do { i <- qvar; return (Var i) }
   <|> do { c <- gcon; return (Con c) }
   <|> do { i <- litInt; return (LitInt i) }
   <|> do { c <- litChar; return (LitChar c) }
   <|> do { s <- litStr; return (LitStr s) }
   <|> do es <- parenList expr
          if length es == 1 then return (head es) else return (Tuple es)
   <|> do es <- bracketList expr
          return (foldr cons (Con "[]") es)
   -- まだいろいろ足りない

alts :: HsParser [(Pat, Rhs)]
alts = braceList alt

alt :: HsParser (Pat, Rhs)
alt = do{ p <- pat
        ; do tokenRArrow
             e <- expr
             ds <- whereDecls
             return (p, Rhs e ds)
      <|> do gds <- many1 (guard tokenRArrow)
             ds <- whereDecls
             return (p, Guarded gds ds)
        }

whereDecls :: HsParser [Decl]
whereDecls = option [] (do { tokenWhere; decls })

guard :: HsParser Token -> HsParser (Expr, Expr)
guard sep = do tokenBar
               gd <- opExp
               sep
               e <- expr
               return (gd, e)

-----------------------------------------------------------------------------
-- patterns

funlhs :: HsParser (String, [Pat])
funlhs = do v <- var
            ps <- many apat
            return (v, ps)

pat :: HsParser Pat
pat = opPat

opPat :: HsParser Pat
opPat = buildExpressionParser (opTable qconopPat) pat10

qconopPat :: Fixity -> HsParser (Pat -> Pat -> Pat)
qconopPat f = do { op <- consymF f; return (\l r -> PCon op [l, r]) }

pat10 :: HsParser Pat
pat10 = do c <- gcon
           ps <- many apat
           return (PCon c ps)
    <|> apat

apat :: HsParser Pat
apat = do v <- var
          option (PVar v) (do { tokenAt; p <- apat; return (AsPat v p) })
   <|> do { c <- gcon; return (PCon c []) }
   <|> do { i <- litInt; return (PInt i) }
   <|> do { c <- litChar; return (PChar c) }
   <|> do { s <- litStr; return (PStr s) }
   <|> do { tokenWildcard; return Wildcard }
   <|> do ps <- parenList pat
          if length ps == 1 then return (head ps) else return (PTuple ps)
   <|> do ps <- bracketList pat
          return (foldr pcons (PCon "[]" []) ps)
   -- <|> ...


-----------------------------------------------------------------------------
-- primitive constructs

gcon :: HsParser String
gcon = qcon

qcon :: HsParser String
qcon = con

con :: HsParser String
con = try (parens consym) <|> conid

qconid :: HsParser String
qconid = conid

conid :: HsParser String
conid = token' test
    where test (TokenConId id) = Just id
          test _               = Nothing

consym :: HsParser String
consym = token' test
    where test (TokenConOp id) = Just id
          test _               = Nothing

consymF :: Fixity -> HsParser String
consymF f = token' test
    where test (TokenConOp id) | fixity id == f = Just id
          test _ = Nothing

vars :: HsParser [String]
vars = sepBy1 var tokenComma

qvar :: HsParser String
qvar = var

var :: HsParser String
var = try (parens varsym) <|> varid

varid :: HsParser String
varid = token' test
    where test (TokenId id) = Just id
          test _            = Nothing

varsym :: HsParser String
varsym = token' test
    where test (TokenOp id) = Just id
          test _            = Nothing

varsymF :: Fixity -> HsParser String
varsymF f = token' test
    where test (TokenOp id) | fixity id == f = Just id
          test _ = Nothing

-- fixity and precedence are now hardcoded
fixity :: String -> Fixity
fixity "."  = InfixR 9
fixity "!!" = InfixL 9
fixity "^"  = InfixR 8
fixity "^^" = InfixR 8
fixity "**" = InfixR 8
fixity "*"  = InfixL 7
fixity "/"  = InfixL 7
fixity "+"  = InfixL 6
fixity "-"  = InfixL 6
fixity ":"  = InfixR 5
fixity "++" = InfixR 5
fixity "==" = InfixN 4
fixity "/=" = InfixN 4
fixity "<"  = InfixN 4
fixity "<=" = InfixN 4
fixity ">=" = InfixN 4
fixity ">"  = InfixN 4
fixity "&&" = InfixR 3
fixity "||" = InfixR 2
fixity ">>" = InfixL 1
fixity ">>="= InfixL 1
fixity "=<<"= InfixR 1
fixity "$"  = InfixR 0
fixity "$!" = InfixR 0
fixity _    = InfixL 9

litInt :: HsParser Int
litInt = token' test
    where test (TokenInt i) = Just i
          test _            = Nothing

litChar :: HsParser String
litChar = token' test
    where test (TokenChar i) = Just i
          test _             = Nothing

litStr :: HsParser String
litStr = token' test
    where test (TokenStr i) = Just i
          test _            = Nothing

tokenIf = hsToken TokenIf
tokenThen = hsToken TokenThen
tokenElse = hsToken TokenElse
tokenLet = hsToken TokenLet
tokenIn = hsToken TokenIn
tokenCase = hsToken TokenCase
tokenOf = hsToken TokenOf
tokenDo = hsToken TokenDo
tokenData = hsToken TokenData
tokenType = hsToken TokenType
tokenClass = hsToken TokenClass
tokenInstance = hsToken TokenInstance
tokenWhere = hsToken TokenWhere
tokenImport = hsToken TokenImport
tokenHiding = hsToken TokenHiding

tokenDotDot = hsToken TokenDotDot
tokenCoco = hsToken TokenCoco
tokenEq = hsToken TokenEq
tokenLambda = hsToken TokenLambda
tokenBar = hsToken TokenBar
tokenLArrow = hsToken TokenLArrow
tokenRArrow = hsToken TokenRArrow
tokenAt = hsToken TokenAt
tokenTilde = hsToken TokenTilde
tokenImply = hsToken TokenImply

tokenLParen = hsToken TokenLParen
tokenRParen = hsToken TokenRParen
tokenLBrace = hsToken TokenLBrace
tokenRBrace = hsToken TokenRBrace
tokenLBracket = hsToken TokenLBracket
tokenRBracket = hsToken TokenRBracket
tokenComma = hsToken TokenComma
tokenSemicolon = hsToken TokenSemicolon
tokenWildcard = hsToken TokenWildcard

-----------------------------------------------------------------------------
-- utilities

token' :: (Token -> Maybe a) -> HsParser a
token' test = token showToken posToken testToken
    where showToken (tok, pos) = show tok
          posToken  (tok, pos) = pos
          testToken (tok, pos) = test tok

satisfy :: (Token -> Bool) -> HsParser Token
satisfy test = token' (\t -> if test t then Just t else Nothing)

hsToken :: Token -> HsParser Token
hsToken tok = satisfy (== tok)

parens :: HsParser a -> HsParser a
parens = hsBetween tokenLParen tokenRParen

brackets :: HsParser a -> HsParser a
brackets = hsBetween tokenLBracket tokenRBracket

parenList :: HsParser a -> HsParser [a]
parenList p = hsBetween tokenLParen tokenRParen (sepBy p tokenComma)

braceList :: HsParser a -> HsParser [a]
braceList p = hsBetween tokenLBrace tokenRBrace (sepBy p tokenSemicolon)

bracketList :: HsParser a -> HsParser [a]
bracketList p = hsBetween tokenLBracket tokenRBracket (sepBy p tokenComma)

hsBetween :: HsParser Token -> HsParser Token -> HsParser a -> HsParser a
hsBetween open close = between open close

test :: Show a => (a -> Bool) -> a -> HsParser a
test f a = if f a then return a else unexpected (show a)

-----------------------------------------------------------------------------
-- expression constructors

arrow :: Type -> Type -> Type
arrow l r = TyAp (TyAp (TyCon "(->)") l) r

ap2 :: Expr -> Expr -> Expr -> Expr
ap2 op l r = Ap (Ap op l) r

bltinApp :: String -> [Expr] -> Expr
bltinApp i es = foldl Ap (Var i) es

cons :: Expr -> Expr -> Expr
cons e1 e2 = (Con ":") `Ap` e1 `Ap` e2

pcons :: Pat -> Pat -> Pat
pcons p1 p2 = PCon ":" [p1, p2]

-----------------------------------------------------------------------------
-- runner

parse :: [(Token, SourcePos)] -> [TopDecl]
parse input = case runParser program () "stdin" input of
                Left err -> error ("parse error at " ++ show err)
                Right tds -> tds

-----------------------------------------------------------------------------
-- test
{-
main = do source <- argf
          parseTest program (lexer "argf" source)

argf :: IO String
argf = do argv <- getArgs
	  if argv == []
	     then getContents
	     else do conts <- mapM getFileContents argv
		     return (concat conts)

getFileContents :: String -> IO String
getFileContents fname = do handle <- IO.openFile fname IO.ReadMode
			   IO.hGetContents handle
-}
