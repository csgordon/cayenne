module System$Char = 
open System$List use (++), (:), Nil, elem in
open System$Bool use Bool, (&&), (||) in
concrete struct
type Char = System$CharType.Char
native ord :: Char -> System$Int.Int = "\\x -> fromEnum (x::Char)"
native chr :: System$Int.Int -> Char = "\\x -> (toEnum x)::Char"
native (==) :: Char -> Char -> System$Bool.Bool = "\\x -> \\y -> x == (y::Char)"
native (/=) :: Char -> Char -> System$Bool.Bool = "\\x -> \\y -> x /= (y::Char)"
native (<=) :: Char -> Char -> System$Bool.Bool = "\\x -> \\y -> x <= (y::Char)"
native (>=) :: Char -> Char -> System$Bool.Bool = "\\x -> \\y -> x >= (y::Char)"
native (<) :: Char -> Char -> System$Bool.Bool = "\\x -> \\y -> x < (y::Char)"
native (>) :: Char -> Char -> System$Bool.Bool = "\\x -> \\y -> x > (y::Char)"
abstract
show :: Char -> System$String.String
show '\n' = "'\\n'"
show '\'' = "'\\''"
show '\\' = "'\\\\'"
show c = System$Bool.if (c < ' ') 
			("'\\" ++ System$Int.show (ord c) ++ "'") 
			('\'' : c : '\'' : Nil)

abstract
isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

abstract
isSpace :: Char -> Bool
isSpace c =
        c == ' ' || c == '\t' || c == '\n' -- || 
--        c == '\r' || c == '\f' || c == '\v' || c == '\xa0'

abstract
isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

abstract
isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

abstract
isSymbol :: Char -> Bool
isSymbol c = elem (==) c "-~!@#$%&*+./<=>?\\^|:¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿×÷"

abstract
isUpper :: Char -> Bool
isUpper c = 
        c >= 'A' && c <= 'Z' || 
        c >= 'À' && c <= 'Ö' || 
        c >= 'Ø' && c <= 'Þ'

abstract
isLower :: Char -> Bool
isLower c = 
        c >= 'a' && c <= 'z' || 
        c >= 'ß' && c <= 'ö' || 
        c >= 'ø' && c <= 'ÿ'

