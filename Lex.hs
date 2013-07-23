module Lex(Token(..), LexItem(..), prLexItem, lexStart, isIdChar) where
-- Lexical analysis.  Written for speed, not beauty!
import Position
import Error
import FString
import Libs.ListUtil
import Data.Char
import Data.Ratio

data LexItem = 
	  L_varid FString
	| L_modid FString
	| L_varsym FString
	| L_integer Integer
	| L_float Rational
	| L_char Char
	| L_string String
	| L_lpar
	| L_rpar
--	| L_comma
	| L_semi
	| L_uscore
	| L_bquote
	| L_lcurl
	| L_rcurl
	| L_lbra
	| L_rbra
	| L_eqlcurl
	| L_eqrcurl
	-- reserved words
	| L_abstract | L_case | L_concrete | L_data | L_do | L_else 
	| L_if | L_import | L_in | L_interface
	| L_let | L_module | L_native | L_of | L_open | L_package | L_postulate | L_private
        | L_public | L_sig | L_struct
        | L_then | L_type | L_use
	-- reserved ops
	| L_dcolon | L_eq | L_at | L_lam | L_bar 
	| L_rarrow | L_larrow | L_brarrow | L_star (Int, Int) | L_dot
	-- pseudo items
	| L_eof StrTable
	| L_error ErrMsg
	deriving (Eq)

prLexItem (L_varid s) = getFString s
prLexItem (L_modid s) = getFString s
prLexItem (L_varsym s) = getFString s
prLexItem (L_integer i) = show i
--prLexItem (L_float r) = prRational 15 r
prLexItem (L_char s) = show s
prLexItem (L_string s) = show s
prLexItem L_lpar = "("
prLexItem L_rpar = ")"
--prLexItem L_comma = ","
prLexItem L_semi = ";"
prLexItem L_uscore = "_"
prLexItem L_bquote = "`"
prLexItem L_lcurl = "{"
prLexItem L_rcurl = "}"
prLexItem L_lbra = "["
prLexItem L_rbra = "]"
prLexItem L_eqlcurl = "={"
prLexItem L_eqrcurl = "={"
prLexItem L_abstract = "abstract"
prLexItem L_case = "case"
prLexItem L_concrete = "concrete"
prLexItem L_data = "data"
prLexItem L_do = "do"
prLexItem L_else = "else"
prLexItem L_if = "if"
prLexItem L_import = "import"
prLexItem L_in = "in"
prLexItem L_interface = "interface"
prLexItem L_let = "let"
prLexItem L_module = "module"
prLexItem L_native = "native"
prLexItem L_of = "of"
prLexItem L_open = "open"
prLexItem L_package = "package"
prLexItem L_postulate = "postulate"
prLexItem L_private = "private"
prLexItem L_public = "public"
prLexItem L_sig = "sig"
prLexItem L_struct = "struct"
prLexItem L_then = "then"
prLexItem L_type = "type"
prLexItem L_use = "use"
prLexItem L_dcolon = "::"
prLexItem L_eq = "="
prLexItem L_at = "@"
prLexItem L_lam = "\\"
prLexItem L_bar = "|"
prLexItem (L_star (n,m)) = "#"++show n++"."++show m
prLexItem L_rarrow = "->"
prLexItem L_larrow = "<-"
prLexItem L_brarrow = "|->"
prLexItem L_dot = "."
prLexItem (L_eof _) = "<EOF>"
prLexItem (L_error s) = "Lexical error: "++show s

data Token = Token Position LexItem deriving (Eq)

type LFlags = (Bool, Bool)

tabStop = 8::Int
nextTab :: Int -> Int
nextTab c = ((c+tabStop-1) `div` tabStop) * tabStop

lexStart :: LFlags -> String -> StrTable -> String -> [Token]
lexStart lflags file tbl cs = lx lflags file 1 0 tbl cs

lx :: LFlags -> String -> Int -> Int -> StrTable -> String -> [Token]
--    flags     file      line   column hashtab     input     output
lx lf f (-1)_ t cs = internalError "lx: unknown position"
lx lf f _ (-1)t cs = internalError "lx: unknown position"
lx lf f l c t ""		= [Token (Position f (l+1) (-1)) (L_eof t)]
lx lf f l c t (' ':cs) 		= lx lf f l (c+1) t cs
lx lf f l c t ('\n':cs) 	= lx lf f (l+1) 0 t cs
lx lf f l c t ('\t':cs) 	= lx lf f l (nextTab (c+1)) t cs
lx lf f l c t ('\r':cs) 	= lx lf f l 0 t cs
lx lf f l c t ('\v':cs) 	= lx lf f l 0 t cs
lx lf f l c t ('\f':cs) 	= lx lf f l 0 t cs
lx lf f l c t ('-':'-':cs) 	= skipToEOL lf f l t cs
lx lf f l c t ('{':'-':cs) 	= skipComm lf (l, c) 1 f l (c+2) t cs
lx lf f l c t ('=':'{':cs)	= Token (Position f l c) L_eqlcurl : lx lf f l (c+2) t cs
lx lf f l c t ('}':'=':cs)	= Token (Position f l c) L_eqrcurl : lx lf f l (c+2) t cs
lx lf f l c t ('(':cs)		= Token (Position f l c) L_lpar : lx lf f l (c+1) t cs
lx lf f l c t (')':cs)		= Token (Position f l c) L_rpar : lx lf f l (c+1) t cs
--lx lf f l c t (',':cs)	= Token (Position f l c) L_comma : lx lf f l (c+1) t cs
lx lf f l c t (';':cs)		= Token (Position f l c) L_semi : lx lf f l (c+1) t cs
lx lf f l c t ('`':cs)		= Token (Position f l c) L_bquote : lx lf f l (c+1) t cs
lx lf f l c t ('{':cs)		= Token (Position f l c) L_lcurl : lx lf f l (c+1) t cs
lx lf f l c t (']':cs)		= Token (Position f l c) L_rbra : lx lf f l (c+1) t cs
lx lf f l c t ('[':cs)		= Token (Position f l c) L_lbra : lx lf f l (c+1) t cs
lx lf f l c t ('}':cs)		= Token (Position f l c) L_rcurl : lx lf f l (c+1) t cs
--lx lf f l c t ('-':'>':cs)	= Token (Position f l c) L_rarrow : lx lf f l (c+2) t cs
--lx lf f l c t (':':':':cs)	= Token (Position f l c) L_dcolon : lx lf f l (c+2) t cs
lx lf f l c t ('\'':cs)		= 
    case lexLitChar' cs of
	Just (cc, n, '\'':cs) -> Token (Position f l c) (L_char cc) : lx lf f l (c+2+n) t cs
	_ -> lexerr f l c t EBadCharLit
lx lf f l c t ('"':cs)		=
	case lexString cs l (c+1) "" of
	    Just (str, l', c', cs') -> Token (Position f l c) (L_string str) : lx lf f l' c' t cs'
	    _ -> lexerr f l c t EBadStringLit
	where
		lexString :: String -> Int -> Int -> String -> Maybe (String, Int, Int, String)
		lexString ('"':s)      l c r             = Just (reverse r, l, c+1, s)
		lexString s            l c r             = lexLitChar' s >>= \ (x, n, s') -> lexString s' l (c+n) (x:r)
lx lf f l c t (x:cs) | isDigit x =
    case span isDigit cs of
    (s', cs') ->
	let s = x:s'
	in  case cs' of
		'.':cs@(y:_) | isDigit y -> 
		    let (s'', cs'') = span isDigit cs
			sf = s++'.':s''
		    in  case cs'' of
			    e:z:w:cs | (e=='e' || e=='E') && (z=='+' || z=='-') && isDigit w -> 
				let (s''', cs''') = span isDigit cs
				in Token (Position f l c) (lfloat (sf++e:z:w:s''')) :
                                   lx lf f l (c+length sf+3+length s''') t cs'''
			    e:w:cs | (e=='e' || e=='E') && isDigit w -> 
				let (s''', cs''') = span isDigit cs
				in Token (Position f l c) (lfloat (sf++e:w:s''')) :
                                   lx lf f l (c+length sf+2+length s''') t cs'''
			    _ -> Token (Position f l c) (lfloat sf) :
                                 lx lf f l (c+length sf) t cs''
		_ -> Token (Position f l c) (L_integer (read s)) : lx lf f l (c+length s) t cs'
{-
lx lf f l c t (x:cs) | isDigit x =
    case span isDigit cs of
    (s', cs') ->
	Token (Position f l c) (L_integer (read (x:s'))) : lx lf f l (c+1+length s') t cs'
-}
lx lf f l c t ('#':ch:cs) | not (isSym ch) =
    let p = Position f l c
    in  if isDigit ch then
            case span isDigit (ch:cs) of
            (s, '.':ch':cs') | isDigit ch' ->
                 case span isDigit (ch':cs') of
                  (s', cs'') -> Token p (L_star (read s, read s')) : lx lf f l (c+1+length s+1+length s') t cs''
            (s, cs') -> let n = read s in Token p (L_star (n,n)) : lx lf f l (c+1+length s) t cs'
        else
            Token p (L_star (0,0)) : lx lf f l (c+1) t (ch:cs)
lx lf f l c t (x:cs) | isSym x = spanSym [] (c+1) cs
    where
    spanSym r cn (y:cs) | isSym y = spanSym (y:r) (cn+1) cs
    spanSym r cn cs' = if cn == (-1::Int) then error "??5" else
	let s = x:reverse r
	    p = Position f l c
	    lxrs x = Token p x : lx lf f l cn t cs'
	in  case s of
		"::"	-> lxrs L_dcolon
		"=" 	-> lxrs L_eq
		"@" 	-> lxrs L_at
		"\\"	-> lxrs L_lam
		"|" 	-> lxrs L_bar
		"#" 	-> lxrs (L_star (0,0))
--		"->"	-> lxrs L_rarrow
		"<-"	-> lxrs L_larrow
--		"|->"	-> lxrs L_brarrow
		"."	-> lxrs L_dot
		_	-> 
			case hmkFString t s of
			(t', fs) ->
			    Token p (L_varsym fs) : lx lf f l cn t' cs'

lx lf f l c t (x:cs) | isAlpha x || x == '_' = spanId [] (c+1) cs
    where
    spanId r cn (y:cs) | isIdChar y = spanId (y:r) (cn+1) cs
    spanId r cn cs' = if cn == (-1::Int) then error "??6" else
	let s = x:reverse r
	    p = Position f l c
	    lxr x = Token p x : lx lf f l cn t cs'
            token = case hmkFString t s of
		    (t', fs) ->
		        if modChar `elem` s then
                            Token p (L_modid fs) : lx lf f l cn t' cs'
			else
			    Token p (L_varid fs) : lx lf f l cn t' cs'
	in  case s of
		"_"		-> lxr L_uscore
		"abstract"	-> lxr L_abstract
		"case"		-> lxr L_case
		"concrete"	-> lxr L_concrete
		"data"		-> lxr L_data
		"do"		-> lxr L_do
		"in"		-> lxr L_in
		"interface"	-> lxr L_interface
		"let"		-> lxr L_let
--		"module"	-> lxr L_module
		-- A hack to allow multiple modules in one file.  
		-- We need to generate a closing '}', so the module keyword has to
		-- be in column -1.
		"module"	-> Token (Position f l (c-1)) L_module : lx lf f l cn t cs'
		"of"		-> lxr L_of
		"open"		-> lxr L_open
		"native"	-> lxr L_native
		"private"	-> lxr L_private
		"public"	-> lxr L_public
		"sig"		-> lxr L_sig
		"struct"	-> lxr L_struct
		"type"		-> lxr L_type
		"use"		-> lxr L_use
		_		-> 
                    case lf of
                    (False, False) -> token
                    (True, _) ->
                        case s of
                        "package"   -> lxr L_package
                        "postulate" -> lxr L_postulate
			"Set"       -> lxr (L_star (0,0))
			"Type"      -> lxr (L_star (1,1))
                        _           -> token
                    (_, True) ->
                        case s of
	                "else"	    -> lxr L_else
                        "if"        -> lxr L_if
		        "import"    -> lxr L_import
		        "then"	    -> lxr L_then
                        _           -> token

lx lf f l c t (x:cs) = lexerr f l c t (EBadLexChar x)

isUpper_ ('_':cs) = isUpper_ cs
isUpper_ (c:_) = isUpper c
isUpper_ [] = True

lexerr f l c t msg = map (Token (Position f l c)) (L_error msg : repeat (L_eof t))

isSym '!' = True; isSym '@' = True; isSym '#' = True; isSym '$' = True
isSym '%' = True; isSym '&' = True; isSym '*' = True; isSym '+' = True
isSym '.' = True; isSym '/' = True; isSym '<' = True; isSym '=' = True
isSym '>' = True; isSym '?' = True; isSym '\\' = True; isSym '^' = True
isSym '|' = True; isSym ':' = True; isSym '-' = True; isSym '~' = True
isSym ',' = True
{-isSym c | c >= '\x80' = c `elem` "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿×÷"-}
isSym c | c >= '\x80' = isSymbol c
isSym _ = False
isIdChar '\'' = True
isIdChar '_' = True
isIdChar c = c == modChar || isAlphaNum c

modChar = '$'

skipComm :: LFlags -> (Int,Int) -> Int -> String -> Int -> Int -> StrTable -> String -> [Token]
skipComm lf lc 0 f l c t cs 	      = lx lf f l c t cs
skipComm lf lc n f l c t ('-':'}':cs) = skipComm lf lc (n-1) f l (c+2) t cs
skipComm lf lc n f l c t ('{':'-':cs) = skipComm lf lc (n+1) f l (c+2) t cs
skipComm lf lc n f l c t ('\n':cs)    = skipComm lf lc n     f (l+1) 0 t cs
skipComm lf lc n f l c t ('\t':cs)    = skipComm lf lc n f l (nextTab (c+1)) t cs
skipComm lf lc n f l c t (_:cs)       = skipComm lf lc n f l (c+1) t cs
skipComm lf (ll,cc) n f l c t ""      = lexerr f l c t (EUntermComm (Position "" ll cc))

skipToEOL :: LFlags -> String -> Int -> StrTable -> String -> [Token]
skipToEOL lf f l t ('\n':cs) = lx lf f (l+1) 0 t cs
skipToEOL lf f l t (_:cs)    = skipToEOL lf f l t cs
skipToEOL lf f l t ""        = lexerr f l 0 t EMissingNL

lexLitChar'		:: String -> Maybe (Char, Int, String)
lexLitChar' ('\\':s)	= lexEsc s
	where
	lexEsc ('x':s)	= let (n,s') = span isHexDigit s in Just (chr (readN 16 n), 2+length n, s')
        lexEsc ('n':s)  = Just ('\n', 1, s)
        lexEsc ('t':s)  = Just ('\t', 1, s)
        lexEsc ('"':s)  = Just ('"', 1, s)
        lexEsc ('\'':s) = Just ('\'', 1, s)
        lexEsc ('\\':s) = Just ('\\', 1, s)
	lexEsc s	= Nothing
lexLitChar' ('\n':_)	= Nothing		-- NL in strings is a bad idea
lexLitChar' (c:s)	= Just (c, 1, s)
lexLitChar' ""		= Nothing

readN radix s = foldl1 (\n d -> n * radix + d) (map ((\c->let n=ord c-ord '0' in if n < 10 then n else n + ord '0' - ord 'A' + 10) . toUpper) s)

maxexp = 10000 :: Int		-- don't allow exponents greater than this since it would take up too much memory

lfloat s =
    let (i,_:de) = span (/= '.') s
	(d, e) = span isDigit de
	r = (read (i++d) % (10 ^ length d))::Rational
	exp = case e of
		"" -> 0
		_:'+':s -> read s
		_:s -> read s
    in  if abs exp > maxexp then
    	    L_error (EExponentRange s)
    	else
	    L_float (r * 10 ^^ exp)

