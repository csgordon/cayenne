module example$mapN =
#include Prelude
struct

data Nat = Zero | Succ Nat

U = #
U1 = #1

--(*) :: #1 -> #1 -> #1
(*) (x, y :: #1) :: #1 = data (,) x y

Un :: Nat -> U1
Un (Zero) = Unit
Un (Succ n) = U * Un n

P :: (n::Nat) -> Un n -> U
P (Zero) _ = Unit
P (Succ n) (X,rho) = X * P n rho

F :: (n::Nat) -> Un n -> U -> U
F n rho A = P n rho -> A

G :: (n::Nat) -> Un n -> U -> U
G (Zero) _ A = A
G (Succ n) (X,rho) A = X -> G n rho A

Curry :: (n::Nat) -> (rho::Un n) -> (A::U) -> (F n rho A) -> (G n rho A)
Curry (Zero) rho A f = f Zero
Curry (Succ n) (X,rho) A f = 
    \ (x :: X) -> Curry n rho A ( \ (u::P n rho) -> f (u,x))

UnCurry :: (n::Nat) -> (rho::U^n) -> (A::U) -> (G n rho A) -> (F n rho A)
UnCurry (Zero) rho A v = \ (_ :: Unit) -> v
UnCurry (Succ n) (X,rho) A g =
   \ (xu :: X x P n rho) -> case xu of (x,u) -> UnCurry n rho A (g x) u

ListN :: (n::Nat) -> Un n -> Un n

ListN (Zero) x = x
ListN (Succ n) (X,rho) = (List X, ListN n rho)

Zip :: (n::Nat) -> (rho :: U^n) -> P n (ListN n rho) -> List (P n rho)
Zip (Zero) _ _ = Nil
Zip (Succ n) (X,rho) (xs,u) =
   zip X (P n rho) xs (Zip n rho u)

zip :: (X::U) -> (Y::U) -> List X -> List Y -> List (X * Y)
zip X Y Nil _ = Nil
zip X Y _ Nil = Nil
zip X Y (x:xs) (y:ys) = (x,y):(zip X Y xs ys)

prod :: (n::Nat) -> (U^n -> U -> U) -> #2
prod (Zero) f = (A::U) -> f Zero A
prod (Succ n) f = (X::U) -> prod n (\ (rho :: Un n) -> f (X,rho))

S :: (n::Nat) ->
 (H:: U^n -> U -> U) -> (L :: (rho::U^n) -> (A::U) -> H rho A) -> prod n H
S (Zero) H L = \ (A::U) -> L Zero A
S (Succ n) H L = \ (X::U) -> S n (\ (rho :: Un n) -> H (X,rho)) (\ (rho :: Un n) -> L (X,rho))

