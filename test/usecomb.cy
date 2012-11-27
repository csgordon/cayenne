module example$usecomb =
#include Prelude
open example$comb use * in

let e = Lam "x" (Lam "f" (App (Var "f") (Var "x")))
    e' = abstractVars e
in  putStrLn (showExp e ++ "\n" ++ showExp e'.exp ++ "\n")

