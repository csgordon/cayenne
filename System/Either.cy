module System$Either = 
open System$String use String in
open System$List use (++) in
struct
data Either a b = Left a | Right b

either :: (a :: #) |-> (b :: #) |-> (c :: #) |->
        (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)  = f x
either f g (Right y) = g y

show :: (a,b :: #) |-> (a->String) -> (b->String) -> Either a b -> String
show f g (Left  x) = "(Left "  ++ f x ++ ")"
show f g (Right y) = "(Right " ++ g y ++ ")"
