module example$usets =
#include Prelude
open example$ts use * in

let tt = \ (a :: #) -> \ (x,y :: a) -> x
    ff = \ (a :: #) -> \ (x,y :: a) -> y
    swap :: (a,b :: #) -> Pair a b -> Pair b a
    swap a b (x,y) = (y,x)
in  --putStrLn (ppExp (tdpe (a' -=> (a' -=> a')) (ff Exp)))
    putStrLn (ppExp (tdpe (TPair a' b' -=> TPair b' a') (swap Exp Exp)))
