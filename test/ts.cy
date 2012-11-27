module example$ts =
#include Prelude
struct
type Id = String

data Exp 
	= EVar Id
	| ELam (Id->Exp)
	| EAp Exp Exp
	| EPair Exp Exp
	| EFst Exp
	| ESnd Exp

data Type
	= TVar
	| TArrow Type Type
	| TPair Type Type

concrete
a' :: Type
a' = TVar

concrete
b' :: Type
b' = TVar

concrete
(-=>) :: Type -> Type -> Type
(-=>) = TArrow

concrete
TypeOf :: Type -> #
TypeOf (TVar) = Exp
TypeOf (TArrow a b) = TypeOf a -> TypeOf b
TypeOf (TPair a b) = Pair (TypeOf a) (TypeOf b)

tdpe :: (t::Type) -> TypeOf t -> Exp
tdpe (TVar) x = x
tdpe (TArrow a b) f = ELam (\(v::Id) -> (tdpe b (f (tdpe' a (EVar v)))))
tdpe (TPair a b) (x,y) = EPair (tdpe a x) (tdpe b y)

tdpe' :: (t::Type) -> Exp -> TypeOf t
tdpe' (TVar) x = x
tdpe' (TArrow a b) f = \ (x :: TypeOf a) -> tdpe' b (EAp f (tdpe a x))
tdpe' (TPair a b) p = (tdpe' a (EFst p), tdpe' b (ESnd p))

private
paren :: Bool -> String -> String
paren (True) s = "(" ++ s ++ ")"
paren (False) s = s

private
pp :: Integer -> Bool -> Exp -> String
pp n p (EVar v) = v
pp n p (ELam e) = 
	let v = "x"++show n
	in  paren p ("\\"++v++"->"++pp (n+1) False (e v))
pp n p (EAp f a) = paren p (pp n True f++" "++pp n True a)
pp n p (EPair x y) = "("++pp n False x++", "++pp n False y++")"
pp n p (EFst e) = pp n p (EAp (EVar "fst") e)
pp n p (ESnd e) = pp n p (EAp (EVar "snd") e)

ppExp :: Exp -> String
ppExp e = pp 0 False e
