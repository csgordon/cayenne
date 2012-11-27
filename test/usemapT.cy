module example$usemapT =
#include Prelude
open example$mapT use * in
let bs = True : False : Nil
    is = 1 : 2 : Nil
    ts = ConsT Bool (OneT Integer)
    xs = mapT ts f bs is
    f :: Bool -> Integer -> Integer
    f b i = if b (negate i) i
in  putStrLn (System$List.show System$Integer.show xs)
