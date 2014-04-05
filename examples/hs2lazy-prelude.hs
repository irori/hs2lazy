--*- haskell -*-
data Maybe a = Nothing | Just a
data Ordering = LT | EQ | GT

type ShowS = String -> String

class Show a where
    showsPrec :: Int -> a -> ShowS
    show      :: a -> String

    showsPrec _ x s = show x ++ s
    show x          = showsPrec 0 x ""

instance Show Int where
    showsPrec p n = if n < 0
                    then showChar '-' . shows (0-n)
                    else let d = chr (ord '0' + mod n 10)
                             m = div n 10
                         in if m == 0
                            then showChar d
                            else showsPrec p m . showChar d

instance Eq a => Eq [a] where
    (==) [] [] = True
    (==) [] _ = False
    (==) _ [] = False
    (==) (x:xs) (y:ys) = x == y && xs == ys
    (/=) xs ys = not (xs == ys)

shows :: (Show a) => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)


($) :: (a -> b) -> a -> b
($) f x = f x

fst :: (a, b) -> a
fst (a, b) = a

snd :: (a, b) -> b
snd (a, b) = b

map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs
map f []     = []

filter :: (a -> Bool) -> [a] -> [a]
filter p []     = []
filter p (x:xs) = if p x
		  then x : filter p xs
                  else filter p xs

null :: [a] -> Bool
null [] = True
null (_:_) = False

head :: [a] -> a
head [] = error "head []"
head (x:xs) = x

tail :: [a] -> [a]
tail [] = error "tail []"
tail (x:xs) = xs

last :: [a] -> a
last [x]    =  x
last (_:xs) =  last xs
last []     =  error "Prelude.last: empty list"

init :: [a] -> [a]
init [x]    = []
init (x:xs) = x : init xs
init []     =  error "Prelude.init: empty list"

length :: [a] -> Int
length []    = 0
length (_:l) = 1 + length l

reverse :: [a] -> [a]
reverse = foldl (\x y -> y : x) []

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

isDigit, isUpper, isLower :: Char -> Bool
isDigit c = let { o = ord c } in o >= ord '0' && o <= ord '9'
isUpper c = let { o = ord c } in o >= ord 'A' && o <= ord 'Z'
isLower c = let { o = ord c } in o >= ord 'a' && o <= ord 'z'

and, or :: [Bool] -> Bool
and = foldr (&&) True
or  = foldr (||) False

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     =  z
foldl f z (x:xs) =  foldl f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z xs = if null xs
	       then z
	       else f (head xs) (foldr f z (tail xs))

elem :: Eq a => a -> [a] -> Bool
elem e [] = False
elem e (x:xs) = e == x || elem e xs

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup x ((key, val) : ys) = if x == key then Just val else lookup x ys
lookup x [] = Nothing

compare :: Int -> Int -> Ordering
compare x y = if x == y then EQ else if x <= y then LT else GT

repeat :: a -> [a]
repeat x = let { xs = x:xs } in xs

not :: Bool -> Bool
not True  = False
not False = True

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith z (a:as) (b:bs) = z a b : zipWith z as bs
zipWith _ _ _           = []

span, break :: (a -> Bool) -> [a] -> ([a],[a])
span p []         = ([],[])
span p xs@(x:xs') = if p x
		    then case span p xs' of
			 (ys, zs) -> (x:ys,zs)
		    else ([], xs)
break p           = span (not . p)

le :: String -> String -> Bool
le [] _          = True
le _ []          = False
le (x:xs) (y:ys) = case compare (ord x) (ord y) of
		   LT -> True
		   EQ -> le xs ys
		   GT -> False

lines    :: String -> [String]
lines [] =  []
lines s  =  case break ((==) '\n') s of
              (l, s') -> l : case s' of
			       []      -> []
			       (_:s'') -> lines s''

unlines          :: [String] -> String
unlines          =  concatMap (\s -> s ++ "\n")

listToMaybe            :: [a] -> Maybe a
listToMaybe []         =  Nothing
listToMaybe (a:_)      =  Just a

find                    :: (a -> Bool) -> [a] -> Maybe a
find p                  =  listToMaybe . filter p

otherwise :: Bool
otherwise = True


data Stream = Stream Char Stream

eof = chr 256

fromStream :: Stream -> String
fromStream (Stream c cs) = if 256 <= ord c then [] else c : fromStream cs

toStream :: String -> Stream
toStream [] = Stream '\n' $ Stream eof (toStream [])
toStream (c:cs) = Stream c (toStream cs)

putStr :: String -> Stream -> Stream
putStr s _ = toStream s

interact :: (String -> String) -> Stream -> Stream
interact f = toStream . f . fromStream


-- Mock IO operations
(>>) :: a -> b -> b
(>>) x y = y
hSetBuffering x y = x
stdout = 1
data Buffering = NoBuffering
