module System$HO = 
open System$Tuples use Pair, (,) in
struct
const :: (a :: #) |-> (b :: #) |-> a -> b -> a
const x y = x

id :: (a :: #) |-> a -> a
id x = x

(·) :: (a, b, c :: #) |->
       (b -> c) -> (a -> b) -> (a -> c)
(·) f g x = f (g x)

--($) :: (a,b :: #) |-> (a -> b) -> a -> b
--($) f x = f x

flip :: (a, b, c :: #) |-> 
        (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

curry :: (a, b, c :: #) |-> 
         (Pair a b -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a, b, c :: #) |-> 
           (a -> b -> c) -> Pair a b -> c
uncurry f (x, y) = f x y
