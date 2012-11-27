module Checker(allChecks) where
import Control.Monad(msum)
import Data.Maybe(maybeToList)
import Util(findDup)
import PPrint
import ISyntax
import Id
import TMonad
import IUtil(patToLam, iFreeVarsL, iBoundVars)

allChecks :: IModule -> Maybe String
allChecks m = msum [
    uniqueBoundVars m,
    noFreeVars m
    ]

-----

uniqueBoundVars :: IModule -> Maybe String
uniqueBoundVars (IModule (i, (t, e, _))) = 
    case findDup (filter (not . isDummyUId) (iBoundVars t ++ iBoundVars e)) of
    [] -> Nothing
    is -> Just ("Bound dups " ++ ppDebug is)

-----

noFreeVars :: IModule -> Maybe String
noFreeVars (IModule (i, (t, e, _))) = 
    case iFreeVarsL t ++ iFreeVarsL e of
    [] -> Nothing
    is -> Just ("Unbound vars " ++ ppDebug is)

-----

{-
eqTst r x y = do
    b <- betaEq r x y 
    if b then return () else err (noPosition, "eqTst")

-- Return the type (in whnf) of a term.
iTChk :: Env -> IExpr -> MT IType
iTChk r (IUniv _ a@(i,t) e) = do
    et <- iTChk (extendEnv i (local t) r) e
    case et of
     IKind _ _ -> return et
     _ -> internalError "iTChk IUniv"
iTChk r (Ilam b a@(i,t) e) = do
    et <- iTChk (extendEnv i (local t) r) e
    cloneExpr (iUniv b a et)
iTChk r (IProduct ss) = do
    let r' = extendEnvs [(i, mdefn t me) | (_, (i, t, me)) <- ss] r
        f (_, (_, t, Nothing)) = iTChk r' t
	f (_, (_, t, Just e))  = do
	    t' <- iTChk r' t
	    te <- iTChk r' e
	    eqTst r' t te
	    return t'
    ts <- mapM f ss
    return (mmKind ts)
iTChk r (IRecord ds) = do
    ss <- mapM (\ (i, (t, e, con)) -> do
                   ui <- mkUniqId i
                   t' <- cloneExpr t
                   oe <- if con then return Nothing else cloneExpr e >>= return . Just
                   return (i, (ui, t', oe))
               ) ds
    return (canonIProduct ss)
iTChk r (ISelect e i) = do
    t' <- iTChk r e
    case t' of
     IProduct as -> case lookup i as of Just (_, t, _) -> iOpenSimpl e as t >>= whnf r
     _ -> internalError "iTChk: ISelect"
iTChk r (ISum _) =
    do  
        ts <- mapM (typeOf r) [ t | (_, ts) <- cs, t <- ts ]
        return (mmKind ts)
iTChk r (ICon _ t) = cloneExpr t -- t is always in WHNF
iTChk r (Icase e _ _ t) = do
    t' <- whnf r t
    case t' of
     IUniv _ (v,vt) tt -> 
         if isDummyUId v then
	     cloneExpr tt
	 else
             whnfClone r (subst v vt e tt)
     _ -> internalError ("iTChk Icase")
iTChk r (IKind n m) = return (IKind (n+1) (m+1))
iTChk r (ILVar v) = case lookupType v r of 
                         Just t -> whnfClone r t
                         _ -> internalError ("iTChk local not found: " ++ ppDebug v ++ ppEnv r)
iTChk r (IGVar v) = case lookupTypeG v r of 
                         Just t -> whnfClone r t
                         _ -> internalError ("iTChk global not found: " ++ ppDebug v ++ ppEnv r)
iTChk r (IApply _ f a) = do
    t' <- iTChk r f
    case t' of
     IUniv _ (i, t) b -> 
         if isDummyUId i then
	     return b
	 else do
	     a' <- cloneExpr a
	     return (subst i t a' b)
     _ -> internalError ("iTChk: IApply " ++ ppDebug t')
iTChk r (Ilet ds e) = do
    t <- iTChk (extendEnvs [(i, (defn t e)) | (i, (t, e)) <- ds] r) e
    whnfClone r (iLetSimpl ds t)
iTChk r (Iletrec ds e) = do
    t <- iTChk (extendEnvs [(i, (defn t e)) | (i, (t, e)) <- ds] r) e
    whnfClone r (iLetrecSimpl ds t)
iTChk r (ILit t _) = cloneExpr t
iTChk r (IWarn _ e) = iTChk r e
iTChk r e = internalError ("no match in iTChk: " ++ ppReadable e)

mmKind [] = iStar
mmKind ts = IKind (maximum [ n | IKind n _ <- ts ]) (minimum [ n | IKind _ n <- ts ])

-}
