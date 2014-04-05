-- Tiny lisp interpreter (https://github.com/irori/lazyk-lisp)

-- hs2lazy ignores import declarations
import Prelude hiding (read, readList)
import Data.Char (ord, isDigit)
import System.IO

------------------
-- Lisp Objects --
------------------
data Object = Cons Object Object
            | Number Int
            | Symbol String
            | Proc ([Object] -> Global -> (Object, Global))

instance Show Object where
   showsPrec 0 (Cons hd tl) = showChar '(' . showsPrec 0 hd . showsPrec 1 tl
   showsPrec 0 (Number n) = showsPrec 0 n
   showsPrec 0 (Proc _) = showString "<procedure>"
   showsPrec 0 (Symbol s) = showString s
   showsPrec 1 (Cons hd tl) = showChar ' ' . showsPrec 0 hd . showsPrec 1 tl
   showsPrec 1 (Symbol "nil") = showChar ')'
   showsPrec 1 a = showString " . " . showsPrec 0 a . showChar ')'

atomQuote = Symbol "quote"
atomNil = Symbol "nil"
atomT = Symbol "t"

isTrue :: Object -> Bool
isTrue (Symbol s) = s /= "nil"
isTrue _ = True

eq (Number n1) (Number n2) = n1 == n2
eq (Symbol s1) (Symbol s2) = s1 == s2
eq _ _ = False

list :: [Object] -> Object
list = foldr Cons atomNil

------------
-- Reader --
------------
read :: String -> (Object, String)
read (' ':s) = read s
read ('\n':s) = read s
read ('(':s) = readList [] s
read ('\'':s) = case read s of
                  (o, s') -> (list [atomQuote, o], s')
read s = case span isDigit s of
           ([], _) -> case break (\c -> elem c " \n()") s of
                        (sym, s') -> (Symbol sym, s')
           (digits, s') -> (Number $ parseInt 0 digits, s')

readList :: [Object] -> String -> (Object, String)
readList os (')':s) = (list (reverse os), s)
readList os s = case read s of
                  (o, s') -> readList (o:os) s'

parseInt :: Int -> String -> Int
parseInt a [] = a
parseInt a (d:ds) = parseInt (a * 10 + ord d - ord '0') ds

------------------
-- Global State --
------------------
type Global = (Maybe Error, DynamicVars)
type Error = String
type DynamicVars = [(String, Object)]

initialGlobal :: Global
initialGlobal = (Nothing, [("set", Proc primSet)
                          ,("nil", atomNil)
                          ,("cons", Proc primCons)
                          ,("car", Proc primCar)
                          ,("cdr", Proc primCdr)
                          ,("atom", Proc primAtom)
                          ,("eq", Proc primEq)
                          ,("+", Proc primPlus)
                          ,("-", Proc primMinus)
                          ,("*", Proc primTimes)
                          ,("/", Proc primDivide)
                          ,("mod", Proc primMod)
                          ])

updateDynamic :: Global -> String -> Object -> Global
updateDynamic (err, (v, o) : gvars) var val =
    if v == var
    then (err, (var, val) : gvars)
    else (err, (var, val) : (v, o) : gvars)

-----------------
-- Environment --
-----------------
type Env = String -> Maybe Object

initialEnv :: Env
initialEnv v = Nothing

extendEnv :: Object -> [Object] -> Env -> Env
extendEnv (Cons (Symbol var) vars) (arg:args) env =
    \v -> if v == var then Just arg else extendEnv vars args env v
extendEnv _ _ env = env
-- FIXME: error if length vars /= length args

makeProcedure :: Object -> Object -> Env -> Object
makeProcedure vars body env =
    Proc (\args glo -> eval (extendEnv vars args env) glo body)

---------------
-- Evaluator --
---------------
errExit :: String -> (Object, Global)
errExit msg = (atomNil, (Just msg, []))

apply :: Object -> [Object] -> Global -> (Object, Global)
apply (Proc proc) args glo = proc args glo
apply _ _ _ = errExit "apply: unknown procedure type"

eval :: Env -> Global -> Object -> (Object, Global)
eval env glo e@(Number _) = (e, glo)

eval env glo@(_, gvars) (Symbol v) =
    case env v of
      Just obj -> (obj, glo)
      Nothing -> case lookup v gvars of
                   Just obj -> (obj, glo)
                   Nothing -> errExit ("unbound variable " ++ v)

eval env glo (Cons (Symbol "lambda") (Cons vars (Cons body _))) =
    (makeProcedure vars body env, glo)

eval env glo (Cons (Symbol "quote") (Cons obj _)) = (obj, glo)

eval env glo (Cons (Symbol "if") (Cons e1 (Cons e2 (Cons e3 _)))) =
    case eval env glo e1 of
      (v, glo') -> eval env glo' (if isTrue v then e2 else e3)

eval env glo e@(Cons _ _) =
    case evalArgs env glo e [] of
      (op:args, glo') -> apply op args glo'

evalArgs :: Env -> Global -> Object -> [Object] -> ([Object], Global)
evalArgs env glo (Cons e es) args =
    case eval env glo e of (arg, glo') -> evalArgs env glo' es (arg:args)
evalArgs _ glo _ args = (reverse args, glo)

-----------------------
-- Builtin Functions --
-----------------------
type Builtin = [Object] -> Global -> (Object, Global)
primSet :: Builtin
primSet (Symbol var : val : []) glo = (val, updateDynamic glo var val)
primSet _ glo = errExit "set: invalid argument"

primCons :: Builtin
primCons (e1:e2:[]) glo = (Cons e1 e2, glo)
primCons _ glo = errExit "cons: invalid argument"

primCar :: Builtin
primCar ((Cons a _):[]) glo = (a, glo)
primCar _ glo = errExit "car: invalid argument"

primCdr :: Builtin
primCdr ((Cons _ b):[]) glo = (b, glo)
primCdr _ glo = errExit "cdr: invalid argument"

primAtom :: Builtin
primAtom ((Cons _ _):_) glo = (atomNil, glo)
primAtom _ glo = (atomT, glo)

primEq :: Builtin
primEq (e1:e2:[]) glo = (if eq e1 e2 then atomT else atomNil, glo)
primEq _ glo = errExit "eq: invalid argument"

primPlus :: Builtin
primPlus (Number n : Number m : []) glo = (Number (n+m), glo)
primPlus _ glo = errExit "+: invalid argument"

primMinus :: Builtin
primMinus (Number n : Number m : []) glo = (Number (n-m), glo)
primMinus _ glo = errExit "-: invalid argument"

primTimes :: Builtin
primTimes (Number n : Number m : []) glo = (Number (n*m), glo)
primTimes _ glo = errExit "*: invalid argument"

primDivide :: Builtin
primDivide (Number n : Number 0 : []) glo = errExit "/: division by zero"
primDivide (Number n : Number m : []) glo = (Number (div n m), glo)
primDivide _ glo = errExit "/: invalid argument"

primMod :: Builtin
primMod (Number n : Number 0 : []) glo = errExit "mod: division by zero"
primMod (Number n : Number m : []) glo = (Number (mod n m), glo)
primMod _ glo = errExit "mod: invalid argument"

----------
-- REPL --
----------
defun :: Global -> String -> Object -> Object -> Global
defun (err, gvars) name vars body =
    (err, (name, makeProcedure vars body initialEnv) : gvars)

repl :: Global -> String -> ShowS
repl glo input = showString "> " .
    case read input of
      (Cons (Symbol "defun") (Cons (Symbol name) (Cons vars (Cons body _))), input') ->
          showString name . showChar '\n' . repl (defun glo name vars body) input'
      (Symbol "", []) -> (\x -> x)  -- End of input
      (e, input') ->
          case eval initialEnv glo e of
            (_, (Just errmsg, _)) -> showString errmsg . showChar '\n' . repl glo input'
            (r, glo') -> shows r . showChar '\n' . repl glo' input'

main = hSetBuffering stdout NoBuffering >>
       interact (\s -> repl initialGlobal s "")
