module example$merge =
#include Prelude
struct

merge :: (a :: #) |-> (a -> a -> Bool) -> List a -> List a -> List a
merge le (Nil) ys = ys
merge le xs@(_ : _) (Nil) = xs
merge le xs@(x : xs') ys@(y : ys') =
    if (le x y)
       (x : merge le xs' ys)
       (y : merge le xs ys')
