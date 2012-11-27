module example$vector =
#include Prelude
open System$Integer use * in
struct

abstract
type Vector (n::Integer) a  = List a

concatV :: (n::Integer) |-> (m::Integer) |-> (a:: #) |-> 
           Vector n a -> Vector m a -> Vector (n+m) a
concatV xs ys = xs ++ ys

zipWithV :: (n::Integer) |-> (a :: #) |-> (b :: #) |-> (c :: #) |-> 
            (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c;
zipWithV f xs ys = zipWith f xs ys

{-
listToVector :: (a :: #) |-> (l :: List a) -> Vector (len l) a
listToVector xs = xs

private
len :: (a :: #) |-> List a -> Integer
len (Nil) = 0
len (_ : xs) = 1 + len xs
-}

mkVector :: (a :: #) |-> (n::Integer) -> (Integer -> a) -> Vector n a
mkVector n f = map f (enumFromTo 0 (n-1))

private
enumFromTo :: Integer -> Integer -> List Integer
enumFromTo n m = if (n > m) (Nil|Integer) (n : enumFromTo (n+1) m)

showVector :: (n::Integer) |-> (a :: #) |-> 
              (a -> String) -> Vector n a -> String
showVector |n |a f v = "Vector#" ++ show n ++ " (" ++ System$List.show f v ++ ")"
