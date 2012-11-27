module example$tinterp$interp =
#include Prelude
open example$tinterp$parser use * in
open example$tinterp$tparser use EExpr in
open example$tinterp$abssyn use showType, emptyTEnv in
open example$tinterp$prelude use prelude in
open example$tinterp$eval use * in


let err :: Pair Int (List String) -> IO Unit
    err (i,ss) = putStrLn ("Error at position "++show i++"\n"++ 
                           concat (intersperse "\n" ss))

    ok :: EExpr prelude.tenv -> IO Unit
    ok e = do Monad_IO 
           putStrLn (eval prelude e.t e.e ++ " :: " ++ showType e.t)

in do Monad_IO
  (args :: List String) <- getArgs
  either err ok (parser prelude (head args))

