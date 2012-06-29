{-# OPTIONS -cpp #-}
{-# LINE 2 "Lexer.x" #-}
module Lexer where
import Numeric (readOct, readHex)
import Char (isUpper, isLower)
import qualified Text.ParserCombinators.Parsec.Pos as Pos

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#else
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
alex_base :: Array Int Int
alex_base = listArray (0,36) [-8,110,115,119,-4,157,190,-38,130,-37,-36,-35,0,-34,208,82,218,255,281,0,-28,319,338,-27,364,457,0,544,639,734,829,924,1042,197,1114,1198,1023]

alex_table :: Array Int Int
alex_table = listArray (0,1453) [0,2,2,2,2,2,-1,8,8,8,9,19,19,0,13,13,13,13,13,13,13,13,13,13,2,36,27,36,36,36,36,24,12,12,36,36,12,6,36,36,14,13,13,13,13,13,13,13,13,13,36,12,36,36,36,36,36,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,12,36,12,0,12,0,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,11,36,12,36,2,2,2,2,2,2,2,2,2,2,-1,15,15,15,15,15,15,15,15,0,0,-1,0,2,0,0,0,0,2,0,0,0,0,5,0,5,5,5,5,0,0,0,5,5,0,5,5,5,-1,0,0,0,0,0,0,0,8,0,5,0,5,5,5,5,5,0,0,0,0,0,0,5,0,5,5,5,5,0,0,0,5,5,0,5,5,5,0,33,33,33,33,33,5,0,0,0,5,0,5,5,5,5,5,0,36,0,36,36,36,36,33,0,0,36,36,0,3,36,36,0,0,0,0,0,5,0,5,0,0,36,5,36,36,36,36,36,7,13,13,13,13,13,13,13,13,13,13,15,15,15,15,15,15,15,15,0,0,0,0,0,0,0,5,36,5,0,0,0,16,0,28,0,0,0,0,0,0,18,0,0,0,0,0,0,17,17,17,17,17,17,17,17,17,17,0,36,0,36,0,0,16,17,17,17,17,17,17,0,0,18,17,17,17,17,17,17,17,17,17,17,0,0,0,0,0,0,0,17,17,17,17,17,17,17,17,17,17,17,17,19,0,0,0,0,0,0,0,0,22,22,22,22,22,22,22,22,22,22,19,17,17,17,17,17,17,0,0,23,23,23,23,23,23,23,23,23,23,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,25,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,0,0,0,0,0,0,0,0,0,0,0,0,0,21,21,21,21,21,21,21,21,21,21,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,20,20,0,0,0,20,0,0,0,0,0,0,0,20,0,0,0,20,0,20,0,20,28,28,26,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,32,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,26,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,32,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,26,28,28,28,28,28,28,28,28,28,28,28,28,28,30,30,30,30,30,30,30,30,30,30,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,32,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,26,28,28,28,28,28,28,28,28,28,28,28,28,28,31,31,31,31,31,31,31,31,31,31,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,32,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,26,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,32,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,33,33,33,33,33,36,0,36,36,36,36,0,0,0,36,36,0,36,36,36,0,0,0,33,0,28,0,0,0,0,36,0,36,36,36,36,36,0,0,29,29,29,29,29,29,29,29,29,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,36,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,28,0,0,0,0,28,28,0,0,0,28,0,0,36,0,36,0,0,28,35,0,0,28,0,28,0,28,0,35,35,35,35,35,35,35,35,35,35,0,0,0,0,0,0,0,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,0,0,0,0,35,0,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,0,0,0,0,0,0,0,0,35,35,35,35,35,35,35,35,35,35,0,0,0,0,0,0,0,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,0,0,0,0,35,0,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,1453) [-1,9,10,11,12,13,10,45,45,45,45,39,39,-1,48,49,50,51,52,53,54,55,56,57,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,9,10,11,12,13,9,10,11,12,13,10,48,49,50,51,52,53,54,55,-1,-1,10,-1,32,-1,-1,-1,-1,32,-1,-1,-1,-1,33,-1,35,36,37,38,-1,-1,-1,42,43,-1,45,46,47,10,-1,-1,-1,-1,-1,-1,-1,45,-1,58,-1,60,61,62,63,64,-1,-1,-1,-1,-1,-1,33,-1,35,36,37,38,-1,-1,-1,42,43,-1,45,46,47,-1,9,10,11,12,13,92,-1,-1,-1,58,-1,60,61,62,63,64,-1,33,-1,35,36,37,38,32,-1,-1,42,43,-1,45,46,47,-1,-1,-1,-1,-1,124,-1,126,-1,-1,58,92,60,61,62,63,64,125,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,-1,-1,-1,-1,-1,-1,-1,124,92,126,-1,-1,-1,79,-1,92,-1,-1,-1,-1,-1,-1,88,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,124,-1,126,-1,-1,111,65,66,67,68,69,70,-1,-1,120,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,97,98,99,100,101,102,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,39,97,98,99,100,101,102,-1,-1,48,49,50,51,52,53,54,55,56,57,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,34,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,97,98,-1,-1,-1,102,-1,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,114,-1,116,-1,118,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,9,10,11,12,13,33,-1,35,36,37,38,-1,-1,-1,42,43,-1,45,46,47,-1,-1,-1,32,-1,34,-1,-1,-1,-1,58,-1,60,61,62,63,64,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,97,98,-1,-1,-1,102,-1,-1,124,-1,126,-1,-1,110,39,-1,-1,114,-1,116,-1,118,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,36) [-1,-1,-1,4,4,4,-1,10,10,10,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,36) [[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAcc (alex_action_10))],[(AlexAccSkip)],[],[],[],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_5))],[],[(AlexAcc (alex_action_6))],[],[(AlexAcc (alex_action_7))],[],[],[],[],[],[],[(AlexAcc (alex_action_8))],[],[],[],[],[],[],[],[(AlexAcc (alex_action_9))],[(AlexAcc (alex_action_9))],[(AlexAcc (alex_action_10))]]
{-# LINE 33 "Lexer.x" #-}

single = mtk (single' . head)
dec	= mtk (TokenInt . read)
oct	= mtk (TokenInt . fst . head . readOct . drop 2)
hex	= mtk (TokenInt . fst . head . readHex . drop 2)
ch	= mtk (TokenChar . unescapeLit)
str	= mtk (TokenStr . unescapeLit)
ide	= mtk ide'
sym	= mtk sym'

unescapeLit = unescape . init . tail
unescape [] = []
unescape ('\\':'a':cs) = '\a' : unescape cs
unescape ('\\':'b':cs) = '\b' : unescape cs
unescape ('\\':'f':cs) = '\f' : unescape cs
unescape ('\\':'n':cs) = '\n' : unescape cs
unescape ('\\':'r':cs) = '\r' : unescape cs
unescape ('\\':'t':cs) = '\t' : unescape cs
unescape ('\\':'v':cs) = '\v' : unescape cs
unescape ('\\':c:cs) = c : unescape cs
unescape (c:cs) = c : unescape cs

single'  '(' = TokenLParen
single'  ')' = TokenRParen
single'  '{' = TokenLBrace
single'  '}' = TokenRBrace
single'  '[' = TokenLBracket
single'  ']' = TokenRBracket
single'  ',' = TokenComma
single'  ';' = TokenSemicolon
single'  '_' = TokenWildcard

ide' "if"   = TokenIf
ide' "then" = TokenThen
ide' "else" = TokenElse
ide' "let"  = TokenLet
ide' "in"   = TokenIn
ide' "case" = TokenCase
ide' "of"   = TokenOf
ide' "do"   = TokenDo
ide' "data" = TokenData
ide' "type" = TokenType
ide' "class" = TokenClass
ide' "instance" = TokenInstance
ide' "where" = TokenWhere
ide' s@(c:_)
    | isUpper c = TokenConId s
    | isLower c = TokenId s
ide' s = error ("unknown token " ++ s)

sym' ".." = TokenDotDot
sym' "::" = TokenCoco
sym' "="  = TokenEq
sym' "\\" = TokenLambda
sym' "|"  = TokenBar
sym' "<-" = TokenLArrow
sym' "->" = TokenRArrow
sym' "@"  = TokenAt
sym' "~"  = TokenTilde
sym' "=>" = TokenImply
sym' s@(':':_) = TokenConOp s
sym' s = TokenOp s

mtk :: (String -> Token) -> AlexPosn -> String -> (Token, AlexPosn)
mtk f p s = (f s, p)

data Token = TokenId String
	   | TokenConId String
	   | TokenChar String
	   | TokenStr String
	   | TokenInt Int
	   | TokenIf
	   | TokenThen
	   | TokenElse
	   | TokenLet
	   | TokenIn
	   | TokenCase
	   | TokenOf
	   | TokenDo
	   | TokenData
	   | TokenType
	   | TokenClass
	   | TokenInstance
	   | TokenWhere

	   | TokenDotDot
	   | TokenCoco
	   | TokenEq
	   | TokenLambda
	   | TokenBar
	   | TokenLArrow
	   | TokenRArrow
	   | TokenAt
	   | TokenTilde
	   | TokenImply

	   | TokenLParen
	   | TokenRParen
	   | TokenLBrace
	   | TokenRBrace
	   | TokenLBracket
	   | TokenRBracket
	   | TokenComma
	   | TokenSemicolon
	   | TokenWildcard

	   | TokenOp String
	   | TokenConOp String
	     deriving (Show, Eq)

lexer :: String -> String -> [(Token, Pos.SourcePos)]
lexer fname = map parsecToken . layout . annotate . alexScanTokens
    where parsecToken (tok, AlexPn _ ln col) = (tok, Pos.newPos fname ln col)

lexer' :: String -> String -> [(Token, Pos.SourcePos)]
lexer' fname = map parsecToken . alexScanTokens
    where parsecToken (tok, AlexPn _ ln col) = (tok, Pos.newPos fname ln col)

data AnToken = Layout Int
	     | Indent Int
	     | Token (Token, AlexPosn)

annotate :: [(Token, AlexPosn)] -> [AnToken]
annotate tps = annotate2 0 tps

annotate1 :: Int -> [(Token, AlexPosn)] -> [AnToken]
annotate1 line [] = []
annotate1 line (t@(tok, AlexPn _ ln col) : tps)
    | line < ln = Indent col : rest
    | otherwise = rest
    where rest = Token t : (next tok) ln tps
	  next TokenLet = annotate2
	  next TokenWhere = annotate2
	  next TokenOf = annotate2
	  next _ = annotate1

annotate2 :: Int -> [(Token, AlexPosn)] -> [AnToken]
annotate2 line tps@((TokenLBrace, _):_) = annotate1 line tps
annotate2 line tps@((_, AlexPn _ ln col):_) = Layout col : annotate1 ln tps

layout :: [AnToken] -> [(Token, AlexPosn)]
layout ts = layout' ts []

nullPosn :: AlexPosn
nullPosn = AlexPn 0 0 0

layout' :: [AnToken] -> [Int] -> [(Token, AlexPosn)]
layout' ts@(Indent n : ts') ms@(m:ms')
    | n == m = (TokenSemicolon, nullPosn) : layout' ts' ms
    | n < m  = (TokenRBrace, nullPosn)  : layout' ts ms'
layout' (Indent n : ts) ms = layout' ts ms
layout' (Layout n : ts) ms@(m:_)
    | n > m     = (TokenLBrace, nullPosn) : layout' ts (n:ms)
    | otherwise = (TokenLBrace, nullPosn)
                  : (TokenRBrace, nullPosn)
                  : layout' (Indent n : ts) ms
layout' (Layout n : ts) [] = (TokenLBrace, nullPosn) : layout' ts [n]
layout' (Token t@(TokenRBrace, _) : ts) (0:ms) = t : layout' ts ms
layout' (Token (TokenRBrace, _) : ts) ms = error "parse-error: `}' expected"
layout' (Token t@(TokenLBrace, _) : ts) ms = t : layout' ts (0:ms)
layout' (Token t : ts) ms = t : layout' ts ms
layout' [] [] = []
layout' [] (m:ms) = (TokenRBrace, nullPosn) : layout' [] ms

alex_action_3 = single 
alex_action_4 = dec 
alex_action_5 = oct 
alex_action_6 = hex 
alex_action_7 = ch 
alex_action_8 = str 
alex_action_9 = ide 
alex_action_10 = sym 
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 35 "GenericTemplate.hs" #-}

{-# LINE 45 "GenericTemplate.hs" #-}

{-# LINE 66 "GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 87 "GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 98 "GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input len, _) ->



		AlexSkip input len

	(AlexLastAcc k input len, _) ->



		AlexToken input len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = check_accs (alex_accept `quickIndex` (s))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		base   = alexIndexInt32OffAddr alex_base s
		(ord_c) = ord c
		offset = (base + ord_c)
		check  = alexIndexInt16OffAddr alex_check offset
		
		new_s = if (offset >= (0)) && (check == ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    (-1) -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (len + (1)) 
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)
	check_accs (AlexAccPred a pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastSkip input (len)
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (i) = i
{-# LINE 1 "wrappers.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn, 	-- current position,
		  Char,		-- previous char
		  String)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
				Just (c, (p', c, s))

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
	deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad

{-# LINE 126 "wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Basic wrapper

{-# LINE 147 "wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.


--alexScanTokens :: String -> [token]
alexScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
	  case alexScan inp 0 of
		AlexEOF -> []
		AlexError _ -> error "lexical error"
		AlexSkip  inp' len     -> go inp'
		AlexToken inp' len act -> act pos (take len str) : go inp'


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

