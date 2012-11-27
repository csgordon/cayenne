module example$tinterp$eval =
open example$tinterp$abssyn use * in
open example$tinterp$iprint use * in
open System$Bool use (&&), if in
open System$Int use Int, (+), (<=) in
open System$Maybe use Maybe, Just, Nothing in
open System$String use * in

private 
struct

interp :: (t :: Type) -> (s :: TEnv) -> VEnv s -> (e :: Expr s t) -> Decode t
interp t s r (Gen (Var vs)) = convertType (s vs.x) t vs.p (r vs.x)
interp t s r (Gen (App as)) = (interp (TArrow as.a t) s r as.f)
                              (interp as.a s r as.e)
interp t s r (Gen (If b e1 e2)) = if (interp TBool s r b) (interp t s r e1)
                                                          (interp t s r e2)
interp t s r (Gen (Fix vs)) = let v :: Decode t
                                  v = interp t (extend s vs.x t) (extendRho s r vs.x t v) vs.f
                              in v
interp t s r (Spec e) = interpSpec t s r e

interpSpec :: (t :: Type) -> (s :: TEnv) -> VEnv s -> (e :: SpecExpr s t) -> Decode t
interpSpec (TInt) s r (IConst i) = i
interpSpec (TBool) s r (BConst b) = b
interpSpec (TArrow a b) s r vs = \ (v :: Decode a) -> interp b (extend s vs.x a) (extendRho s r vs.x a v) vs.f


public
eval :: (s :: Env) -> (t :: Type) -> (e :: Expr s.tenv t) -> String
eval s t e = print t (interp t s.tenv s.venv e)
