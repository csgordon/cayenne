module example$tinterp_pe$eval =
open example$tinterp_pe$abssyn use * in
open example$tinterp_pe$iprint use * in
open System$Bool use (&&), if in
open System$Int use Int, (+), (<=) in
open System$Maybe use Maybe, Just, Nothing in
open System$String use * in

private 
struct

interp :: (t :: Type) -> (s :: TEnv) -> (e :: Expr s t) ->  -- Static part
     VEnv s -> Decode t                                     -- Dynamic part
interp t s (Gen (Var vs)) = let c = convertType (s vs.x) t vs.p
                            in \(r :: VEnv s) -> c (r vs.x)
interp t s (Gen (App as)) = let f = interp (TArrow as.a t) s as.f
                                a = interp as.a s as.e
                            in \r -> (f r) (a r)
interp t s (Gen (If b e1 e2)) = let bv = interp TBool s b
                                    e1v = interp t s e1
                                    e2v = interp t s e2
                                in \r -> if (bv r) (e1v r) (e2v r)
interp t s (Gen (Fix vs)) = let vv = interp t (extend s vs.x t) vs.f
                            in \r ->
                            let v :: Decode t
                                v = vv (extendRho s r vs.x t v)
                            in v
interp t s (Spec e) = interpSpec t s e

interpSpec :: (t :: Type) -> (s :: TEnv) -> (e :: SpecExpr s t) -> 
     VEnv s -> Decode t
interpSpec (TInt) s (IConst i) = \_ -> i
interpSpec (TBool) s (BConst b) = \_ -> b
interpSpec (TArrow a b) s vs = let f = interp b (extend s vs.x a) vs.f
                               in \r -> 
                                  \v -> f (extendRho s r vs.x a v) 

public
eval :: (s :: Env) -> (t :: Type) -> (e :: Expr s.tenv t) -> String
eval s t e = print t (interp t s.tenv e s.venv)
