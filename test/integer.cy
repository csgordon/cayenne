module example$integer =
open System$Bool use * in
open System$Integer use * in
open System$IO use * in

let fac :: Integer -> Integer
    fac n = if (n == 0) (1::Integer) (n * fac(n - (1::Integer)))
in putStrLn (show (fac 100))
