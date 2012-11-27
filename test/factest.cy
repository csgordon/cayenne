module example$factest =
#include Prelude

let fac :: Integer -> Integer;
    fac n =
        if (n == 0) 1 (n * fac(n - 1));

    fromTo :: Integer -> Integer -> List Integer;
    fromTo n m =
        if (n <= m) 
           (n : (fromTo (n+1) m)) 
           Nil;
in putStrLn (System$List.show show (map fac (fromTo 1 10)))
