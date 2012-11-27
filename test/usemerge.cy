module example$usemerge =
#include Prelude
let xs = 1 : 2 : 5 : Nil
    ys = 3 : 3 : 4 : Nil
    ms = example$merge.merge |Integer (<=) xs ys
in  putStrLn (System$List.show show ms)
