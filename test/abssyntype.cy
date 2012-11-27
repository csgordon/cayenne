module example$interp$abssyntype =
open System$Char use Char in
open System$Int use Int in
open System$Bool use Bool, (&&), False, True in
open System$Maybe use Maybe, Just, Nothing in

open example$xlogic use Absurd, absurd, Truth, truth, Lift, AndI, (/\), (&) in
struct

data Type = TInt | TBool | TArrow Type Type

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

reflType :: (t :: Type) -> EqType t t
reflType (TInt) = truth
reflType (TBool) = truth
reflType (TArrow a r) = reflType a & reflType r

{-
EqType :: Type -> Type -> #
EqType t1 t2 = Lift (eqType t1 t2)

reflType :: (t :: Type) -> EqType t t
reflType (TInt) = truth
reflType (TBool) = truth
reflType (TArrow a r) = AndI (eqType a a) (eqType r r) 
                             (reflType a) (reflType r)

XXX "abssyn.cy", line 44, column 54, Type error: Cannot normalize reflType a

Same with substType:

substType :: (F :: Type -> #) ->
             (t :: Type) -> (t' :: Type) -> EqType t t' -> F t -> F t'
substType F (TInt) (TInt) _ f = f
substType F (TInt) _ p _ = absurd p
substType F (TBool) (TBool) _ f = f
substType F (TBool) _ p _ = absurd p
substType F (TArrow a r) (TArrow a' r') p f =
    let par :: (EqType a a') /\ (EqType r r')
        par = andProp (eqType a a') (eqType r r') p
    in substType (\ (t::Type) -> F (TArrow t r')) a a' (fst par)
      (substType (\ (t::Type) -> F (TArrow a t)) r r' (snd par) f)
substType F (TArrow _ _) _ p _ = absurd p

-}

substType :: (F :: Type -> #) ->
             (t :: Type) -> (t' :: Type) -> EqType t t' -> F t -> F t'
substType F (TInt) (TInt) _ f = f
substType F (TInt) _ p _ = absurd p
substType F (TBool) (TBool) _ f = f
substType F (TBool) _ p _ = absurd p
substType F (TArrow a r) (TArrow a' r') (pa & pr) f =
    substType (\ t -> F (TArrow t r')) a a' pa
   (substType (\ t -> F (TArrow a t)) r r' pr f)
substType F (TArrow _ _) _ p _ = absurd p

convertType :: (t :: Type) -> (t' :: Type) -> EqType t t' -> Decode t -> Decode t'
convertType = substType Decode


concrete
Decode :: Type -> #
Decode (TInt) = Int
Decode (TBool) = Bool
Decode (TArrow s t) = Decode s -> Decode t
