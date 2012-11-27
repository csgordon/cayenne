module System$Tuples =
open System$String use String in
open System$List use (++) in
struct
data Pair a b = (,) a b

fst :: (a,b :: #) |-> Pair a b -> a
fst (x, y) = x

snd :: (a,b :: #) |-> Pair a b -> b
snd (x, y) = y

show :: (a,b :: #) |-> (a->String) -> (b->String) -> Pair a b -> String
show f g (x, y) = "("++f x++", "++g y++")"

--data Triple a b c = triple a b c;
