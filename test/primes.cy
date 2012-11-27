module example$primes =
#include Prelude
let sieve :: List Integer -> List Integer
    sieve (Nil) = Nil
    sieve (p : ns) = p : sieve (filter (\ (n::Integer) -> rem n p /= 0) ns)
    from :: Integer -> List Integer
    from n = n : from (n+1)
    primes :: List Integer
    primes = sieve (from 2)
in  putStrLn (System$List.show show (take 100 primes))

