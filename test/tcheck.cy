module example$interp$tcheck =
open example$xlogic use (/\), (&) in 
open example$interp$abssyntype use * in
open example$interp$abssyn use * in
open example$interp$ienv use * in
open example$xlogic use Absurd, absurd, Truth, truth in
open System$Maybe use Maybe, Just, Nothing in
open System$Bool use (&&), if in

private 
struct

public concrete
HasType :: Expr -> Type -> TEnv -> #
HasType (Var x) t s = EqType (s x) t
HasType (App f a) t s = sig { ta :: Type; pf :: HasType f (TArrow ta t) s; pa :: HasType a ta s }
HasType (Lam x _ e) (TArrow ta tr) s = HasType e tr (extendT s x ta)
HasType (IConst _) (TInt) _ = Truth
HasType (BConst _) (TBool) _ = Truth
HasType (IPlus x y) (TInt) s = HasType x TInt s /\ HasType y TInt s
HasType (ILE x y) (TBool) s = HasType x TInt s /\ HasType y TInt s
HasType (BAnd x y) (TBool) s = HasType x TBool s /\ HasType y TBool s
HasType _ _ _ = Absurd

public concrete
type TCheck (e :: Expr) (s :: TEnv) = sig { t :: Type; p :: HasType e t s }

public
typeCheck :: (e :: Expr) -> (s :: TEnv) -> Maybe (TCheck e s)
typeCheck e@(Var x) s = Just struct { t = s x; p :: HasType e t s = reflType t }
typeCheck (App f a) s = checkApp f a s (typeCheck f s) (typeCheck a s)
typeCheck (Lam x t e) s = checkLam x t e s (typeCheck e (extendT s x t))
typeCheck e@(IConst _) _ = Just struct { t = TInt;  p :: HasType e t s = truth }
typeCheck e@(BConst _) _ = Just struct { t = TBool; p :: HasType e t s = truth }
typeCheck (IPlus e1 e2) s = checkPlus e1 e2 s (typeCheck e1 s) (typeCheck e2 s)
typeCheck (ILE e1 e2) s = checkLE e1 e2 s (typeCheck e1 s) (typeCheck e2 s)
typeCheck (BAnd e1 e2) s = checkAnd e1 e2 s (typeCheck e1 s) (typeCheck e2 s)

checkLam :: (x :: Symbol) -> (tx :: Type) -> (e :: Expr) -> (s :: TEnv) -> 
            Maybe (TCheck e (extendT s x tx)) -> Maybe (TCheck (Lam x tx e) s)
checkLam x tx e s (Just r) = 
    let pp :: HasType e r.t (extendT s x tx) = r.p -- XXX r.p cannot be substituted for pp
    in Just struct { t = TArrow tx r.t; p ::  HasType (Lam x tx e) t s = pp }
checkLam _ _ _ _ _ = Nothing

checkApp :: (f :: Expr) -> (a :: Expr) -> (s :: TEnv) -> 
            Maybe (TCheck f s) -> Maybe (TCheck a s) -> Maybe (TCheck (App f a) s)
checkApp f a s (Just rf) (Just ra) =
    checkArrow f s rf.t rf.p (\ (tfa, tfr :: Type) -> \ (rpf :: HasType f (TArrow tfa tfr) s) ->
       case eqType' ra.t tfa of {
          (Nothing) -> Nothing|((TCheck (App f a) s)); -- XXX if this arm is removed, we get: "tcheck.cy", line 54, column 85, Undefined identifier: "raEqtfa"
          (Just raEqtfa) -> Just struct { t = tfr; 
		                          p :: HasType (App f a) t s 
			                    = struct { ta = tfa; pf = rpf; 
                                                       pa = hasAlsoType a s ra.t tfa raEqtfa ra.p } };
            }  )
checkApp _ _ _ _ _ = Nothing

hasAlsoType :: (e :: Expr) 
            -> (s :: TEnv) 
            -> (t :: Type) 
            -> (t' :: Type) 
            -> EqType t t' 
            -> HasType e t s 
            -> HasType e t' s
hasAlsoType e s t t' p f = substType (\ (t::Type) -> HasType e t s) t t' p f
-- XXX the following doesn't type check
-- hasAlsoType e s = substType (\ (t::Type) -> HasType e t s)

checkArrow :: (a :: #) |-> (f :: Expr) -> (s :: TEnv) -> 
              (t :: Type) -> HasType f t s -> ((ta, tr :: Type) -> HasType f (TArrow ta tr) s -> Maybe a) -> Maybe a
checkArrow f s (TArrow a r) p f = f a r p
checkArrow f s _ _ _ = Nothing

checkPlus :: (e1 :: Expr) -> (e2 :: Expr) -> (s :: TEnv) -> 
             Maybe (TCheck e1 s) -> Maybe (TCheck e2 s) -> Maybe (TCheck (IPlus e1 e2) s)
checkPlus e1 e2 s (Just r1) (Just r2) =
    checkInt e1 s r1.t r1.p (\ (p1 :: HasType e1 TInt s) -> 
    checkInt e2 s r2.t r2.p (\ (p2 :: HasType e2 TInt s) ->
    Just struct { t = TInt; p :: HasType (IPlus e1 e2) t s = p1 & p2 }
    ))
checkPlus _ _ s _ _ = Nothing

checkLE :: (e1 :: Expr) -> (e2 :: Expr) -> (s :: TEnv) -> 
             Maybe (TCheck e1 s) -> Maybe (TCheck e2 s) -> Maybe (TCheck (ILE e1 e2) s)
checkLE e1 e2 s (Just r1) (Just r2) =
    checkInt e1 s r1.t r1.p (\ (p1 :: HasType e1 TInt s) -> 
    checkInt e2 s r2.t r2.p (\ (p2 :: HasType e2 TInt s) ->
    Just struct { t = TBool; p :: HasType (ILE e1 e2) t s = p1 & p2 }
    ))
checkLE _ _ s _ _ = Nothing

checkAnd :: (e1 :: Expr) -> (e2 :: Expr) -> (s :: TEnv) -> 
             Maybe (TCheck e1 s) -> Maybe (TCheck e2 s) -> Maybe (TCheck (BAnd e1 e2) s)
checkAnd e1 e2 s (Just r1) (Just r2) =
    checkBool e1 s r1.t r1.p (\ (p1 :: HasType e1 TBool s) -> 
    checkBool e2 s r2.t r2.p (\ (p2 :: HasType e2 TBool s) ->
    Just struct { t = TBool; p :: HasType (BAnd e1 e2) t s = p1 & p2 }
    ))
checkAnd _ _ s _ _ = Nothing

checkInt :: (a :: #) |-> (e :: Expr) -> (s :: TEnv) -> 
            (t :: Type) -> HasType e t s -> (HasType e TInt s -> Maybe a) -> Maybe a
checkInt e s (TInt) p f = f p
checkInt e s _ _ _ = Nothing

checkBool :: (a :: #) |-> (e :: Expr) -> (s :: TEnv) -> 
            (t :: Type) -> HasType e t s -> (HasType e TBool s -> Maybe a) -> Maybe a
checkBool e s (TBool) p f = f p
checkBool e s _ _ _ = Nothing
