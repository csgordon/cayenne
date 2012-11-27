module System$Logic =
open System$Bool use * in
struct

data Absurd =

absurd :: (a :: #) |-> Absurd -> a
absurd i = case i of { }

data Truth = truth

data (/\) a b = (&) a b

data (\/) a b = Inl a | Inr b

type (<=>) a b = sig { impR :: a->b; impL :: b->a; }

concrete
Lift :: Bool -> #
Lift (False) = Absurd
Lift (True)  = Truth

concrete
LiftBin :: (a:: #) |-> (a -> a -> Bool) -> (a -> a -> #)
LiftBin |a op = \(x::a) -> \(y::a) -> Lift (op x y)

