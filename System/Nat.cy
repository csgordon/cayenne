module System$Nat = 
open System$Integer use Integer, (<), (-) in
open System$Int use Int in
open System$Bool use if, True, False, Bool in
open System$Error use error in
struct
data Nat = Zero | Succ Nat

concrete
(+) :: Nat -> Nat -> Nat
(+) (Zero) m = m
(+) (Succ n) m = Succ (n + m)

concrete
(==) :: Nat -> Nat -> Bool
(==) (Zero) (Zero) = True
(==) (Succ n) (Succ m) = n == m
(==) _ _ = False

toInteger :: Nat -> Integer
toInteger (Zero) = 0
toInteger (Succ n) = System$Integer.(+) (toInteger n) 1

fromInteger :: Integer -> Nat
fromInteger i = if (i < 0) 
                   (error |Nat "fromInteger < 0")
                   (if (System$Integer.(==) i 0)
                       Zero
                       (Succ (fromInteger (i-1))))

toInt :: Nat -> Int
toInt (Zero) = 0
toInt (Succ n) = System$Int.(+) (toInt n) 1

-- XXX should have some more useful functions on Nat
