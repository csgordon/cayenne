module Error(EMsg, ErrMsg(..), prEMsg, prError, internalError, unimpl, flatErrors, eMany) where
import Data.List(intercalate, sortBy)
import Position
import PPrint

type EMsg = (Position, ErrMsg)
data ErrMsg 
	= ESyntax String [String]	-- found token, expected tokens
	| EBadCharLit
	| EBadStringLit
	| EBadLexChar Char
	| EUntermComm Position
	| EMissingNL
	| EUnbound String
	| EDuplicate Position String
	| EDuplicateCon String
{-10-}	| ETypeError String String String String String Position
	| EKindError String String String String Position
	| ETypeTermination String String
	| EFieldNonKind String String
	| EConNonKind String String
	| EVarNonKind String String
	| ENotSum String String
	| ENotSumElem String String
	| ENotProduct String String
	| ENotProductOpen String String
{-20-}	| ENotProductElem String String
	| ENotSumCase String String
	| ENotInType String String
	| ENoSign String
	| EBadRecursion String
        | ETermination String
        | EHide Bool String
	| ENoHide String
	| EOverlap Position
	| EBadPatterns String
{-30-}	| EMissingPatArg String
        | EExtraPatArg String
	| ENoArms String
	| ETooManyArgs String
	| EVaryingArgs String
	| EVaryingHiddenArgs String
	| ENoLamSign String
	| ENotHidden
	| ENotProductCoerce String String
	| ENotProductCoerceT String String
{-40-}	| ENoCoerce String String
        | EConUnknown String
	| EExponentRange String
	| EConstantRange String String
	| EBadHole

	| EMany [EMsg]
	| ETEMP String

{-1-}	| WNoMatch
	| WBoundConstr String
	deriving (Eq, Ord)

instance Show ErrMsg where
--    showsType _ = showString "Errmsg"
    showsPrec _ e = showString "ErrMsg:" . showString (prError e)

instance PPrint ErrMsg where
    pPrint _ _ e = text (show e)

prEMsg (p,m) = prPosition p++", "++prError m
-- 1
prError (ESyntax s ss) = "Syntax error: found token "++s++if null ss then "" else ", expected "++unwordsOr ss
prError EBadCharLit = "Bad Char literal"
prError EBadStringLit = "Bad String literal"
prError (EBadLexChar c) = "Bad character in input: "++show c
prError (EUntermComm p) = "Unterminated {- comment, started at "++prPosition p
prError EMissingNL = "Missing \"\\n\" after -- comment"

prError (EUnbound i) = "Undefined identifier: "++ishow i
prError (EDuplicate p i) = "Duplicate definition of "++ishow i++", other definition at "++prPosition p
prError (EDuplicateCon i) = "Duplicate constructor names: "++ishow i
prError (ETypeError s msg e t1 t2 pos) =
    let s' = "["++s++"]"
    in "Type error: "++s++"\nExpression\n"++e++"  has type\n"++t1++"  but should have type\n"++t2++
       (if pos /= noPosition then "  from " ++ prPosition pos ++ "\n" else "") ++
       msg
prError (EKindError msg e t1 t2 pos) =
    "Kind error: "++"\nType\n"++e++"  has kind\n"++t1++"  which is not contained in\n"++t2++
    (if pos /= noPosition then "  from " ++ prPosition pos ++ "\n" else "") ++
    msg
prError (ETypeTermination e t) = 
    "Type error: Cannot normalize \n"++t
prError (EFieldNonKind t i) = "Field type illegal: "++ i ++" :: " ++ t
prError (EConNonKind t i) = "Constructor field type illegal: " ++ t
prError (EVarNonKind t i) = "Variable type illegal: " ++ i ++ " :: " ++ t
prError (ENotSum t i) = "Constructor type is not a sum: " ++ t
prError (ENotSumElem t i) = "Constructor type does not contain constructor: " ++ i ++ " in " ++ t
prError (ENotProduct t i) = "Type of selection expression ( ."++i++" ) not a product: " ++ t
prError (ENotProductOpen t i) = "Type of open expression not a product: " ++ t
prError (ENotProductElem t i) = "Product type does not contain selector: " ++ i ++ " in " ++ t
prError (ENotSumCase t i) = "Pattern matching on a non-data type: " ++ t
prError (ENotInType t i) = "Type does not contain constructor: " ++ i ++ " in " ++ t
prError (ENoSign i) = "Definition must have a (complete) type signature: " ++ i
prError (EBadRecursion e) = "Illegal recursion in definition: " ++ e
prError (ETermination e) = "No termination in:\n" ++ e
prError (EHide b e) = "Argument should " ++ (if b then "" else "not ") ++ "be hidden: " ++ e
prError (ENoHide i) = "Cannot figure out hidden argument: " ++ i
prError (EOverlap pos) = "Overlapping pattern, overlaps with pattern at " ++ prPosition pos
prError (EBadPatterns e) = "Bad patterns: " ++ e
prError (EMissingPatArg i) = "Missing argument to pattern constructor: " ++ i
prError (EExtraPatArg i) = "Extra argument to pattern constructor: " ++ i
prError (ENoArms e) = "Cannot figure out type of empty case: " ++ e
prError (ETooManyArgs p) = "More function arguments than the function type specifies: " ++ p
prError (EVaryingArgs i) = "Varying number of arguments to function: " ++ i
prError (EVaryingHiddenArgs i) = "Varying hiding of arguments to function: " ++ i
prError (ENoLamSign i) = "Variable must have a type signature: " ++ i
prError (ENotHidden) = "Hiding marker on non-hidden argument"
prError (ENotProductCoerce t e) = "Expression not a product: " ++ e ++ ", has type " ++ t
prError (ENotProductCoerceT t' t) = "Type not a product: " ++ t ++ "= " ++ t'
prError (ENoCoerce t e) = "Impossible coercion: " ++ e ++ " :: " ++ t
prError (EConUnknown e) = "Constructor must have explicit type: " ++ e
prError (EExponentRange s) = "Preposterous floating literal: "++show s
prError (EConstantRange t s) = "Literal out of range for type "++show t++": "++s
prError (EBadHole) = "Duplicated or misplaced []"

prError (EMany es) = "Many:\n" ++ unlines (map prEMsg es)
prError (ETEMP s) = "TEMP: "++s

prError (WNoMatch) = "Warning: Missing cases, pattern matching may fail"
prError (WBoundConstr i) = "Warning: Variable in pattern with same the name as a constructor: " ++ i
--prError _ = internalError "Missing error message."

internalError msg = error ("Internal compiler error: "++msg)

ishow :: String -> String
ishow s = "\"" ++ s ++ "\""

flatErrors :: [EMsg] -> [EMsg]
flatErrors es = flatErrors' noPosition es
flatErrors' p es = sortBy (\ (Position _ l1 _, _) (Position _ l2 _, _) -> l1 `compare` l2) (concatMap f es)
	where f (p, EMany es') = flatErrors' p es'
	      f e@(p',m) = if p' == noPosition then [(p, m)] else [e]

unimpl s = internalError (s++" is not implemented yet")

plural s  [_] = s
plural ""  _ = "s"
plural "y" _ = "ies"
plural "s" _ = "es"

unwordsAnd = unwordsx "and"
unwordsOr = unwordsx "or"

unwordsx _ [] = ""
unwordsx _ [x] = x
unwordsx s [x1, x2] = unwords [x1, s, x2]
unwordsx s xs = intercalate ", " (init xs++[s++" "++last xs])

eMany es = (noPosition, EMany es)

