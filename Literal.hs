module Literal(Literal(..)) where
import Position
import PPrint
import Rational(prRational)

data Literal 
	= LString String 
        | LChar Char
        | LInt Integer
        | LInteger Integer
        | LFloat Rational
	| LNative String
	| LImpossible
	| LNoMatch Position
        deriving (Eq, Ord)


instance PPrint Literal where
    pPrint _ _ (LString s) = text (show s)
    pPrint _ _ (LChar s) = text (show s)
    pPrint _ _ (LInt s) = text (show s)
    pPrint _ _ (LInteger s) = text (show s)
    pPrint _ _ (LFloat s) = text (prRational 15 s)
    pPrint _ _ (LNative s) = text (show s)
    pPrint _ _ LImpossible = text "_impossible"
    pPrint _ _ (LNoMatch p) = text "_noMatch"
