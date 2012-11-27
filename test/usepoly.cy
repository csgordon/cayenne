module example$usepoly =
#include Prelude
open example$Regular use * in
open example$PMap use * in
open example$Base use * in
open example$testBase use * in
open example$Tree use Tree, Bin, Leaf in

let plus1 (i :: Integer) = i+1
    showIntegerList = System$List.show  System$Integer.show
    showIntegerTree = example$Tree.show System$Integer.show
in do Monad_IO
     putStrLn (showIntegerList (pmap  ListType plus1 (1:2:3:Nil)))
     putStrLn (showIntegerTree (pmap  TreeType plus1 (Bin (Leaf 1) (Leaf 2))))
     putStrLn (showIntegerTree (pmapc TreeType plus1 (Bin (Leaf 1) (Leaf 2))))
     putStrLn (showIntegerTree (pmapa TreeType plus1 (Bin (Leaf 1) (Leaf 2))))
     putStrLn (showIntegerTree (pid |Integer TreeType (Bin (Leaf 1) (Leaf 2))))
