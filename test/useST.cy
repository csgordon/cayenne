module example$useST =
#include Prelude
open System$ST use * in

let
  scomp :: ((s::ST) -> s.M Int)
  scomp s = open s use * in
    do m
     (v :: Var Int) <- newVar 41
     (i :: Int) <- readVar v
     writeVar v (i + 1)
     (w :: Var Int) <- newVar 24
     (i :: Int) <- readVar w
     writeVar w (i + 1)
     scomp2 s v w
  -- need to break up to avoid compilation complexity
  scomp2 :: ((s::ST) -> s.Var Int -> s.Var Int -> s.M Int)
  scomp2 s v w = open s use * in
    do m
     readVar v
     (i :: Int) <- readVar w
     (x :: Int) <- readVar w
     let y = sameVar v v
     if (sameVar v v && sameVar w w && not (sameVar v w) && i == 25)
         (m.return unit) (writeVar v 0)
     readVar v

in putStr (System$Int.show (runST scomp) ++ "\n")
