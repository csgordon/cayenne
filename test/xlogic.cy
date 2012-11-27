-- Some logic definitions
module example$xlogic =
open System$Bool use * in
open System$Tuples use * in
struct

data Absurd =

data Truth = truth

absurd :: (a :: #) |-> Absurd -> a
absurd i = case i of { }

data (/\) a b = (&) a b

data (\/) a b = Inl a | Inr b

type (<=>) a b = sig { impR :: a->b; impL :: b->a; }

concrete
Lift :: Bool -> #
Lift (False) = Absurd
Lift (True)  = Truth

concrete
AndI :: (a,b :: Bool) -> Lift a -> Lift b -> Lift (a && b)
AndI (True) (True) pa pb = truth
AndI (True) (False) pa pb = absurd pb
AndI (False) _ pa pb = absurd pa

concrete
andProp :: (a,b :: Bool) -> Lift (a && b) -> Lift a /\ Lift b
andProp (True) (True) _ = truth & truth
andProp (True) (False) p = absurd p
andProp (False) _ p = absurd p

concrete
LiftBin :: (a:: #) |-> (a -> a -> Bool) -> (a -> a -> #)
LiftBin |a op = \(x::a) -> \(y::a) -> Lift (op x y)

