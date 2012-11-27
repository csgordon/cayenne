-- Useful definitions on relations
module example$equiv =
concrete struct

Rel :: (a :: #) -> #1
Rel a = a -> a -> #

Refl :: (a :: #) |-> Rel a -> #
Refl |a R = (x::a) -> x `R` x

Symm :: (a :: #) |-> Rel a -> #
Symm |a R = (x,y::a) -> x `R` y -> y `R` x

Trans :: (a :: #) |-> Rel a -> #
Trans |a R = (x,y,z::a) -> x `R` y -> y `R` z -> x `R` z

Cong :: (a :: #) |-> Rel a -> #1.0
Cong |a R = (F :: a -> #) |-> (x,y :: a) -> x `R` y -> F x -> F y

Equiv :: (a :: #) |-> Rel a -> #1.0
Equiv R = sig
   refl  :: Refl R
   symm  :: Symm R
   trans :: Trans R
   cong  :: Cong R
