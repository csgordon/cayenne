module example$interp$interp =
#include Prelude
open example$interp$abssyntype use * in
open example$interp$abssyn use * in
open example$interp$eval use * in
let i41 = IConst 41
    i1 = IConst 1
    x = Var 'x'
    fun = Lam 'x' TInt (IPlus x i1)
    e = App fun i41
in  case eval e of
    (Nothing) -> putStrLn "Type error"
    (Just s) -> putStrLn s
