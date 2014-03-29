--*- Haskell -*-
{
module Lexer where
import Numeric (readOct, readHex)
import Data.Char (isUpper, isLower)
import qualified Text.ParserCombinators.Parsec.Pos as Pos
}

%wrapper "posn"

$l = [a-zA-Z]				-- letters
$d = 0-9				-- digits
$i = [$l$d\_\']				-- identifier character
$s = [!\#\$\%&\*\+\-\.\/:\<=>\?@\\\|\~]	-- symbolic char
@e = \\([abfnrtv\"\'\\]|$d{1,3})	-- character escape
@c = $printable#[\"\\]|@e		-- string character
@g = \\$white+\\			-- string gap

tokens :-

  $white+			;  -- white space
  "--".*			;  -- line comment
  "{-"([~\-]|\-+[~\-\}]|\n)*\-+\}	;  -- comment
  [\,\(\)\[\]_\{\;\}]		{ single }  -- single char token
  $d+				{ dec }  -- signed integer literal
  0[oO][0-7]+			{ oct }  -- signed octal literal
  0[xX][0-9a-fA-F]+		{ hex }  -- signed hexadecimal literal
  \'(@c|\")\'			{ ch }  -- character literal
  \"(@c|@g)*\"			{ str }  -- string literal
  $l$i*				{ ide }  -- alphabetic identifier
  $s+				{ sym }  -- symbolic identifier

{

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
ide' "import" = TokenImport
ide' "hiding" = TokenHiding
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
           | TokenImport
           | TokenHiding

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
}
