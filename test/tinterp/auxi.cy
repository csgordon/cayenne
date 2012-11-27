module example$tinterp$aux =
open example$tinterp$abssyn use * in
open System$Int use Int in
concrete struct

private 
spec :: (s :: TEnv) -> (t :: Type) -> SpecExpr s t -> Expr s t
spec s t e = Spec@(Expr s t) e

iconst :: (s :: TEnv) |-> Int -> Expr s TInt
iconst |s i = spec s TInt (IConst@(SpecExpr s TInt) i)

lam :: (s :: TEnv) |-> (a,b :: Type) -> (x :: Symbol) 
    -> Expr (extend s x a) b -> Expr s (TArrow a b)
lam |s a b x' f' = spec s (TArrow a b) struct{ x = x'; f = f' }

var :: (s :: TEnv) -> (t :: Type) -> (x :: Symbol)
    -> EqType (s x) t -> Expr s t
var s t x' p' = Gen (Var@(GenExpr s t) struct{ x = x'; p = p' })

app :: (s :: TEnv) |-> (a, t :: Type) -> Expr s (TArrow a t) -> Expr s a
    -> Expr s t
app |s a' t f' e' = Gen (App@(GenExpr s t) struct{ a = a'; f = f'; e = e' })

if :: (s :: TEnv) |-> (t :: Type) -> Expr s TBool -> Expr s t -> Expr s t
   -> Expr s t
if |s t b e1 e2 = Gen (If@(GenExpr s t) b e1 e2)

fix :: (s :: TEnv) |-> (t :: Type) -> (x :: Symbol)
    -> Expr (extend s x t) t -> Expr s t
fix |s t x' e = Gen (Fix@(GenExpr s t) struct{ x=x'; f = e })

(==>) = TArrow
