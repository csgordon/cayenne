module example$io =
#include Prelude
    do Monad_IO
        (args :: List String) <- getArgs
        putStr "arg 1 is '"
        putStr (head args)
        putStr "'\n"
