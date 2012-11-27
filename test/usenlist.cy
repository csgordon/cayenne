module example$usenlist =
#include Prelude
open System$Int use * in
open example$nlist use * in
let l = cons 1 (cons 2 (cons 3 nil))
in  do Monad_IO
    putStrLn (show (length l))
    --putStrLn (show (hd l))

