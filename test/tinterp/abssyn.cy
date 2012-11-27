module example$tinterp$abssyn =
open example$logic use Absurd, absurd, Truth, truth, Lift, AndI, (/\), (&) in
open System$Int use Int in
open System$Bool use Bool, (&&), False, True, if, IF in
open System$Maybe use Maybe, Just, Nothing in
open System$String use String in
open System$StringUtil use (==) in
open System$List use (++) in

struct

type Symbol = String

data Type = TInt | TBool | TArrow Type Type

showType :: Type -> String
showType (TInt) = "Int"
showType (TBool) = "Bool"
showType (TArrow t1 t2) = "(" ++ showType t1 ++ " -> " ++ showType t2 ++ ")"

type TEnv = Symbol -> Type

concrete
emptyTEnv :: TEnv
emptyTEnv = \ (s :: Symbol) -> TInt

--concrete
extend :: TEnv -> Symbol -> Type -> TEnv
extend s x t = \ (y :: Symbol) -> if (x == y) t (s y)

type VEnv (s :: TEnv) = (x :: Symbol) -> Decode (s x)

emptyVEnv :: VEnv emptyTEnv
emptyVEnv = \ (s :: Symbol) -> 0

extendRho :: (s :: TEnv) ->
             (r :: VEnv s) ->
             (x :: Symbol) ->
             (t :: Type) ->
             (v :: Decode t) ->
	     (VEnv (extend s x t))

extendRho s r x t v = \ (y :: Symbol) -> IF (x == y) v (r y)

type Env = sig { tenv :: TEnv; venv :: VEnv tenv }

emptyEnv :: Env
emptyEnv = struct { tenv = emptyTEnv; venv = emptyVEnv }

extends :: Symbol -> (t :: Type) -> (v :: Decode t) -> Env -> Env
extends x t v env = struct{ tenv = extend env.tenv x t; 
                            venv = extendRho env.tenv env.venv x t v }

data Expr (s :: TEnv) (t :: Type) = Gen (GenExpr s t) | Spec (SpecExpr s t)

data GenExpr (s :: TEnv) (t :: Type) = 
      Var (sig { x :: Symbol; p :: EqType (s x) t })
    | App (sig { a :: Type; f :: Expr s (TArrow a t); e :: Expr s a })
    | If (Expr s TBool) (Expr s t) (Expr s t)
    | Fix (sig { x :: Symbol; f :: Expr (extend s x t) t })

type SpecExpr (s :: TEnv) (t :: Type) =
   case t of
      (TInt)       -> IntExpr s
      (TBool)      -> BoolExpr s
      (TArrow a b) -> FunExpr s a b

data IntExpr (s :: TEnv) = 
    IConst Int

data BoolExpr (s :: TEnv) =
    BConst Bool

type FunExpr (s :: TEnv) (a :: Type) (b :: Type) =
    sig { x :: Symbol; f :: Expr (extend s x a) b }

eqType :: Type -> Type -> Bool
eqType (TInt) (TInt) = True
eqType (TBool) (TBool) = True
eqType (TArrow a1 r1) (TArrow a2 r2) = eqType a1 a2 && eqType r1 r2
eqType _ _ = False

concrete
EqType :: Type -> Type -> #
EqType (TInt) (TInt) = Truth
EqType (TBool) (TBool) = Truth
EqType (TArrow a1 r1) (TArrow a2 r2) = (EqType a1 a2) /\ (EqType r1 r2)
EqType _ _ = Absurd

eqType' :: (t :: Type) -> (t' :: Type) -> Maybe (EqType t t')
eqType' (TInt) (TInt) = Just truth
eqType' (TBool) (TBool) = Just truth
eqType' (TArrow a1 r1) (TArrow a2 r2) = case (eqType' a1 a2 & eqType' r1 r2) of
                                             ((Just p1) & (Just p2)) -> Just (p1 & p2)
                                             _                       -> Nothing
eqType' _ _ = Nothing

concrete
refType :: (t :: Type) -> EqType t t
refType (TInt) = truth
refType (TBool) = truth
refType (TArrow a r) = refType a & refType r



substType :: (F :: Type -> #) ->
             (t :: Type) -> (t' :: Type) -> EqType t t' -> F t -> F t'
substType F (TInt) (TInt) _ f = f
substType F (TInt) _ p _ = absurd p
substType F (TBool) (TBool) _ f = f
substType F (TBool) _ p _ = absurd p
substType F (TArrow a r) (TArrow a' r') (pa & pr) f =
    substType (\ (t::Type) -> F (TArrow t r')) a a' pa
   (substType (\ (t::Type) -> F (TArrow a t)) r r' pr f)
substType F (TArrow _ _) _ p _ = absurd p

convertType :: (t :: Type) -> (t' :: Type) -> EqType t t' -> Decode t -> Decode t'
convertType = substType Decode

concrete
Decode :: Type -> #
Decode (TInt) = Int
Decode (TBool) = Bool
Decode (TArrow s t) = Decode s -> Decode t
