module example$useapply =
#include Prelude
open example$apply use * in

let f0 :: Integer
    f0 = 4
    f1 :: Integer -> Integer
    f1 x = x + 1
    f2 :: Integer -> Integer -> Integer
    f2 x y = x * 10 + y
    val :: Integer
    val = apply |Integer |Integer (apply |Integer |Integer (apply |Integer |Integer (Nil|Integer) f0 : Nil) f1
                 : 1 : Nil) f2
in  putStrLn (show val)

