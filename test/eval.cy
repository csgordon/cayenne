module example$interp$eval =
open example$xlogic use Absurd, absurd, Truth, truth, (/\), (&) in
open example$interp$abssyntype use * in
open example$interp$abssyn use * in
open example$interp$ienv use * in
open example$interp$tcheck use * in
open example$interp$iprint use * in
open System$Bool use (&&), if in
open System$Int use Int, (+), (<=) in
open System$Maybe use Maybe, Just, Nothing in
open System$String use * in

private 
struct

fst :: (a, b :: #) |-> (a /\ b) -> a
fst (a & b) = a

snd :: (a, b :: #) |-> (a /\ b) -> b
snd (a & b) = b

interp :: (e :: Expr) -> (t :: Type) -> (s :: TEnv) -> VEnv s -> HasType e t s -> Decode t
interp (Var x) t s r p = convertType (s x) t p (r x)
interp (App f a) t s r p = (interp f (TArrow p.ta t) s r p.pf) 
                           (interp a p.ta            s r p.pa)
interp (Lam x _ e) (TArrow ta tr) s r p = \ v -> interp e tr (extendT s x ta) (extendV s r x ta v) p
interp (Lam _ _ _) _ _ _ p = absurd p
interp (IConst i) (TInt) s r _ = i
interp (IConst _) _ _ _ p = absurd p
interp (BConst b) (TBool) s r _ = b
interp (BConst _) _ _ _ p = absurd p
interp (IPlus x y) (TInt) s r p = interp x TInt s r (fst p) + interp y TInt s r (snd p)
interp (IPlus _ _) _ _ _ p = absurd p
interp (ILE x y) (TBool) s r p = interp x TInt s r (fst p) <= interp y TInt s r (snd p)
interp (ILE _ _) _ _ _ p = absurd p
interp (BAnd x y) (TBool) s r p = interp x TBool s r (fst p) && interp y TBool s r (snd p)
interp (BAnd _ _) _ _ _ p = absurd p

eval' :: (e :: Expr) -> (mtc :: Maybe (TCheck e emptyTEnv)) -> Maybe String
eval' e (Nothing) = Nothing
eval' e (Just r) = Just (print r.t (interp e r.t emptyTEnv emptyVEnv r.p))

-- Evaluate an expression returning Nothing after type checking if it is bad.
public
eval :: Expr -> Maybe String
eval e = eval' e (typeCheck e emptyTEnv)

