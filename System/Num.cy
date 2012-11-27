module System$Num =
open System$Nat use Nat, Succ, Zero in
open System$Int use Int in

concrete
struct

data Num = NonNeg Nat | Neg Nat

abstract
toInt :: Num -> Int
toInt (NonNeg n) = System$Nat.toInt n
toInt (Neg n) = System$Int.(-) 0 (System$Nat.toInt (Succ n))

(+) :: Num -> Num -> Num
(+) (NonNeg n) (NonNeg m) = NonNeg (System$Nat.(+) n m)
(+) (NonNeg n) (Neg m) = sub n (Succ m)
(+) (Neg n) (NonNeg m) = sub m (Succ n)
(+) (Neg n) (Neg m) = Neg (System$Nat.(+) (Succ n) (Succ m))

(-) :: Num -> Num -> Num
(-) (NonNeg n) (NonNeg m) = sub n m
(-) (NonNeg n) (Neg m) = NonNeg (System$Nat.(+) n (Succ m))
(-) (Neg n) (NonNeg m) = Neg (System$Nat.(+) (Succ n) m)
(-) (Neg n) (Neg m) = sub (Succ m) (Succ n)

sub :: Nat -> Nat -> Num
sub (Zero) (Zero) = NonNeg Zero
sub (Zero) (Succ n) = Neg n
sub (Succ n) (Zero) = NonNeg (Succ n)
sub (Succ n) (Succ m) = sub n m


neg :: Num -> Num
neg (NonNeg (Succ n)) = Neg n
neg (Neg n) = NonNeg (Succ n)
neg n = n

succ = (+) (NonNeg (Succ Zero))

zero = NonNeg Zero
one = succ zero
two = succ one
minusone = neg one
minustwo = neg two
