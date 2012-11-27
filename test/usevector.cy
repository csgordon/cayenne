module example$usevector =
#include Prelude
open System$Integer use * in
open example$vector use * in

let x = mkVector 5 (\ (i::Integer) -> i)
    y = mkVector 2 (\ (i::Integer) -> i)
    z = mkVector 3 (\ (i::Integer) -> i)
    yz = concatV y z
    xyz = zipWithV (+) x yz
in  putStrLn (showVector show xyz)
