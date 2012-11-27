-- Useful definitions on relations
module example$equiv =
open System$Error use undefined in
open example$xlogic use (<=>) in
concrete struct

Rel :: (a :: #) -> #1
Rel a = a -> a -> #

Refl :: (a :: #) |-> Rel a -> #
Refl |a R = (x::a) -> x `R` x

Symm :: (a :: #) |-> Rel a -> #
Symm |a R = (x,y::a) -> x `R` y -> y `R` x

Trans :: (a :: #) |-> Rel a -> #
Trans |a R = (x,y,z::a) -> x `R` y -> y `R` z -> x `R` z

Cong :: (a :: #) |-> Rel a -> #
Cong |a R = (F :: a -> #) -> (x,y :: a) -> x `R` y -> F x <=> F y

Equiv :: (a :: #) |-> Rel a -> #
Equiv R = sig
   refl  :: Refl R
   symm  :: Symm R
   trans :: Trans R
--   cong  :: Cong R

cie :: (a :: #) |-> (R :: Rel a) -> Cong |a R -> Equiv |a R
cie |a R cong = struct
   refl :: Refl R
   refl = undefined
   symm :: Symm R
   symm x y xRy =
     let yRy :: y `R` y = (cong (\ (w::a) -> w `R` y) x y xRy).impR xRy
     in  (cong (\ (w::a) -> y `R` w) x y xRy).impL yRy
   trans :: Trans R
   trans x y z xRy yRz =
     (cong (\ (w::a) -> x `R` w) y z yRz).impR xRy
