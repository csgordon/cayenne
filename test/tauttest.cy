module example$tauttest =
#include Prelude
open example$taut use Nat, Zero, Succ, taut, TautArg in

let id :: Bool -> Bool
    id x = x

    implies :: Bool -> Bool -> Bool
    implies x y = not x || y

    fun :: Bool -> Bool -> Bool
    fun x y = implies x y || implies y x
in  do Monad_IO
    putStrLn (System$Bool.show (taut Zero True))
    putStrLn (System$Bool.show (taut (Succ Zero) id))
    putStrLn (System$Bool.show (taut (Succ (Succ Zero)) fun))
    putStrLn (System$Bool.show (taut (Succ Zero) (\ (x::Bool) -> x || not x)))
    putStrLn (System$Bool.show (taut (Succ (Succ Zero)) (\ (x,y :: Bool) -> x `implies` y || x)))
    putStrLn (System$Bool.show (taut (Succ (Succ Zero)) (\ (x,y :: Bool) -> x && y)))
    putStrLn (System$Bool.show (taut Zero False))
