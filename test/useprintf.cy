module example$useprintf =
#include Prelude
open example$printf use * in
do Monad_IO
    putStr (printf "Hello\n")
    putStr (printf "%d %d\n" 123 44)
    putStr (printf "%s %d %s\n" "hej" 99 "hopp")
