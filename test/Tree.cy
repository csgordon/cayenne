module example$Tree =
open System$String use String in
open System$List use (++) in
struct

data Tree a = Leaf a | Bin (Tree a) (Tree a)
----------------------------------------------------------------
-- show for trees
show :: (a :: #) |-> (a->String) -> Tree a -> String;
show s (Leaf x)  = "Leaf "++s x;
show s (Bin l r) = "Bin ("++show s l++") ("++show s r++")"
