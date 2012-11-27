module example$monad = 
#include Prelude
let xys = do Monad_List
             (x::Integer) <- 2 : (3 : Nil)
             (y::Integer) <- 4 : (5 : Nil)
             return (x*y)
in  putStrLn (System$List.show show xys)

