module example$tinterp_pe$lex = 

open System$Char use * in
open System$List use * in
open System$String use String in
open System$StringUtil use (==) in
open System$Bool use * in
open System$Int use read, Int, (+) in
open System$Tuples use Pair, (,) in
open System$Unit use Unit, unit in
open System$HO use (·) in
open System$Maybe use Maybe, Just, Nothing in

struct

data Lex = 
   LSym String
 | LId String
 | LKW String
 | LInt Int
 | L_LP
 | L_RP
 | L_EOF

isInt :: Lex -> Maybe Int
isInt (LInt i) = Just i
isInt _ = Nothing

isSym :: Lex -> Maybe String
isSym (LSym s) = Just s
isSym _ = Nothing

isId :: Lex -> Maybe String
isId (LId s) = Just s
isId _ = Nothing

isKW :: String -> Lex -> Maybe String
isKW s (LKW s') = if (s == s') (Just s) Nothing
isKW _ _ = Nothing

isLP :: Lex -> Maybe Unit
isLP (L_LP) = Just unit
isLP _ = Nothing

isRP :: Lex -> Maybe Unit
isRP (L_RP) = Just unit
isRP _ = Nothing

isEOF :: Lex -> Maybe Unit
isEOF (L_EOF) = Just unit
isEOF _ = Nothing

type Tokens = List (Pair Int Lex)

lex :: String -> Tokens
lex = map kws · lex0 0

lex0 :: Int -> String -> Tokens
lex0 i (Nil) = (i,L_EOF) : Nil
lex0 i ('(':cs) = (i,L_LP) : lex0 (i+1) cs
lex0 i (')':cs) = (i,L_RP) : lex0 (i+1) cs
lex0 i ccs@(c:cs) = if (isSpace c) (lex0 (i+1) cs)
                   (if (isDigit c) (lex' i isDigit (LInt·read) i ccs)
                   (if (isAlpha c) (lex' i isAlphaNum LId i ccs)
                                   (lex' i isSymbol LSym i ccs)))

lex' :: Int -> (Char -> Bool) -> (String -> Lex) -> Int -> String -> Tokens
lex' = lex'' Nil

lex'' :: String -> Int -> (Char -> Bool) -> (String -> Lex) -> Int -> String -> Tokens
lex'' r i0 p l i (Nil) = (i0,l (reverse r)) : lex0 i Nil
lex'' r i0 p l i ccs@(c:cs) = if (p c) (lex'' (c:r) i0 p l (i+1) cs)
                                       ((i0,l (reverse r)) : lex0 i ccs)


kws :: Pair Int Lex -> Pair Int Lex
kws (i,l@(LSym s)) = (i,if (elem (==) s sym_kws) (LKW s) l)
kws (i,l@(LId s)) = (i,if (elem (==) s id_kws) (LKW s) l)
kws il = il

sym_kws = "\\" : "::" : "->" : Nil

id_kws = "Int" : "Bool" : "if" : "then" : "else" : "fix" : Nil
