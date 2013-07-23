module IReduce(whnf, betaEq, cloneExpr, typeOfX, typeOf, iOpenSimpl, univKind {-, kindOf, levelOf-}) where
import Data.List(union, intersect, (\\), partition)
import Control.Monad
import Data.Maybe
import Libs.ListUtil(mapSnd, lookupWithDefault)
import PPrint
import Position(noPosition)
import Error
import Id
import MiscId
import ISyntax
import ISyntaxPP
import IUtil
import Env
import TMonad
import qualified UIdSet as S
import Util(sortFst, doFull)

import Util(traces, tracex, tracex2, doTrace2)

infixr 3  &&&

whnf :: Env -> IExpr -> MT IExpr
whnf r e = do
    n <- getCount
    tracex2 ("enter whnf: cnt=" ++ show n ++ " " ++ ppDebug e ++ "env:\n" ++ ppEnv r ++ "-----\n") $ return ()
--    e' <- reduceCount 50 (whnf' r e)
    e' <- whnf' r e
    n' <- getCount
    tracex2 ("exit whnf: " ++ ppDebug e ++ "  --->\n" ++ ppDebug e') $ return ()
    if n /= n' then tracex2 ("whnf count=" ++ show (n-n') ++ " " ++ ppReadable(e, e')) $ return () else return ()
    return e'

whnf' :: Env -> IExpr -> MT IExpr
whnf' r (ISelect e i) = do
    step
    e' <- whnf r e
    case e' of
     IRecord ds ->
	let (_,x,_) = lookupWithDefault ds 
	      (internalError ("whnf: can't find "++ppString i++
	       " in "++ppString e'++"("++ppString (ISelect e i)++")")) i
	in  whnf r x
     _ -> do
        t <- typeOf r e
        case t of
	 IProduct ss | mval /= Nothing -> 
	     tracex2 ("whnf-select: " ++ ppReadable (e, e', t)) $
	     whnfClone r (iNSubst s val)
		where mval = findVal i ss
		      Just val = mval
		      findVal v ss = head ([ e | (i', (_,_, e)) <- ss, i'==i ] ++ [Nothing])
		      s = [(ui, ISelect e' i) | (i, (ui, _, _)) <- ss]
         _ -> return (ISelect e' i)
whnf' r ec@(Icase e arms d t) = do
    step
    e' <- whnf r e
    case spine [] e' of
     (ICon c _, es) -> 
	case lookup (ICCon c) arms of
	Nothing  -> whnf r d
	Just rhs -> let rhs' = patToLam' rhs in
	    tracex2 ("case: " ++ ppAll ec ++ "-->\n" ++ ppAll (foldl iApply rhs' es) ++ "----\n") $
	    whnf r (foldl iApply rhs' es)
     (ILit p LImpossible, []) -> return (ILit p LImpossible)
     (ILit p l, es) | notNative l -> 
	case lookup (ICLit p l) arms of
	Nothing  -> whnf r d
	Just rhs -> let rhs' = patToLam' rhs in
	    tracex2 ("case: " ++ ppAll ec ++ "-->\n" ++ ppAll (foldl iApply rhs' es) ++ "----\n") $
	    whnf r (foldl iApply rhs' es)
     (ILVar v, []) | mcs /= Nothing && null arms' -> whnf r d
        where mcs = lookupNonCons v r
	      Just cs = mcs
	      arms' = [ a | a@(c, _) <- arms, c `notElem` cs]
{-
-- XXX using this causes congr.cy to loop
     (Icase ce carms cd ct, []) -> do
        let fArm (c, (vs, e)) = do
                ec' <- cloneExpr (Icase e arms d t)
		ec'' <- {-whnf' r-}return ec'
                return (c, (vs, ec''))
        carms' <- mapM fArm carms
        cd' <- {-whnf' r-}(cloneExpr (Icase cd arms d t))
        ret <- {-whnf' r-}return (Icase ce carms' cd' t)
        tracex ("case-of-case "++ ppDebug (ec, e', ret)) $ return ()
        return ret
-}
     _ -> return (Icase e' arms d t)

-- Handle integer arithmetic.
whnf' r e@(IApply _ (IApply _ f e1) e2) | isJust mp = do
    step
    e1' <- whnf r e1
    e2' <- whnf r e2
    case fromJust mp e1' e2' of
     Just e' -> return e'
     Nothing -> return e
 where mp = primBinOp r f

whnf' r (IApply h f a) = do
    step
    f' <- whnf r f
    case f' of
     Ilam _ (v,t) e -> 
       tracex2 ("apply: " ++ ppDebug (IApply h f a) ++ "f=\n" ++ ppDebug f' ++ "-->\n" ++ ppDebug (subst v t a e) ++ "----\n") $
       whnf r (subst v t a e)
     _ -> return (IApply h f' a)
whnf' r e@(IProduct ds) = return e
whnf' r e@(IRecord ds) = return e
whnf' r e@(ILVar v) =
    case lookupValue v r of
    Just e' -> do step; whnfClone r e'
    Nothing -> return e
whnf' r e@(IGVar v) =
    case lookupValueG v r of
    Just e' -> do step; whnfClone r e'
    Nothing -> return e
whnf' r (Ilet [] b) = whnf r b
whnf' r (Ilet ((i,(t,e)):ds) b) = do
    step
    w <- whnf (extendEnv i (defn t e) r) (Ilet ds b)
    pushLet (addLetBind (i,(t,e)) w)
whnf' r (Iletrec ds b) = do
    step
    w <- whnf (extendEnvs [(i, (defn t e)) | (i, (t, e)) <- ds] r) b
    pushLet (Iletrec ds w)
whnf' r (IWarn _ e) = whnf' r e -- XXX drop warning?
whnf' r (ILit st (LString "")) = return (ICon nilId stringDataType)
whnf' r (ILit st (LString (c:cs))) = return $
    IApply False (IApply False (ICon consId stringDataType) (ILit charType (LChar c))) (ILit st (LString cs))
whnf' r e = return e

stringDataType = ISum [(nilId, []), (consId, [(us, charType), (us, stringType)])]
  where us = dummyUId noPosition

-----

alphaEq :: IExpr -> IExpr -> Bool
alphaEq x y = alphaEq' [] x y

alphaEq' :: [(UId, UId)] -> IExpr -> IExpr -> Bool
alphaEq' iis x y = x == y || 
    case (x, y) of
    (IUniv _ (xi, xt) xe, IUniv _ (yi, yt) ye) -> aeq xt yt && alphaEq' ((xi,yi):iis) xe ye
    (Ilam  _ (xi, xt) xe, Ilam  _ (yi, yt) ye) -> aeq xt yt && alphaEq' ((xi,yi):iis) xe ye
    (IProduct xss,        IProduct yss)        -> let iis' = zip [u | (_,(u,_,_)) <- xss] 
                                                                 [u | (_,(u,_,_)) <- yss] ++ iis
                                                  in  length xss == length yss && and (
                                                       zipWith (\ (xl,(_,xt,mxe)) (yl,(_,yt,mye)) ->
                                                                xl == yl && alphaEq' iis' xt yt &&
                                                                case (mxe, mye) of
                                                                (Nothing, Nothing) -> True
                                                                (Just xe, Just ye) -> alphaEq' iis' xe ye
                                                                _ -> False) xss yss)
    (IRecord xds,         IRecord  yds)        -> length xds == length yds && and (
                                                   zipWith (\ (xi, (xt, xe, xc)) (yi, (yt, ye, yc)) ->
                                                            xi == yi && aeq xt yt && aeq xe ye && xc == yc)
                                                   xds yds)
    (ISelect xe xi,       ISelect ye yi)       -> xi == yi && aeq xe ye
    (ISum xcs,            ISum ycs)            -> length xcs == length ycs && and (
                                                   zipWith (\ (xc, xts) (yc, yts) -> xc == yc && 
                                                            aeqCS iis xts yts) xcs ycs)
						  where aeqCS _ [] [] = True
							aeqCS iis ((xi, xt):xits) ((yi, yt):yits) =
							    aeq xt yt && aeqCS ((xi,yi):iis) xits yits
							aeqCS _ _ _ = False
    (ICon xc xt,          ICon yc yt)          -> xc == yc && aeq xt yt
    (Icase xe xas xd xt,  Icase ye yas yd yt)  -> aeq xe ye && aeq xd yd && aeq xt yt && 
                                                  length xas == length yas && and (
                                                   zipWith (\ (xc, (xvs, xe)) (yc, (yvs, ye)) ->
                                                            xc == yc && length xvs == length yvs &&
                                                            alphaEq' (zip (map fst xvs) 
                                                                          (map fst yvs) ++ iis) xe ye)
                                                   xas yas)
--  IKind, by == above
    (ILVar xi,            ILVar yi)            -> lookupWithDefault iis xi xi == yi
--  IGVar, by == above
    (IApply _ xf xa,      IApply _ yf ya)      -> aeq xf yf && aeq xa ya
    (Ilet xds xe,         Ilet yds ye)         -> f iis xds yds
                                                  where f iis [] [] = alphaEq' iis xe ye
                                                        f iis ((xi,(xt,xe)):xds) ((yi,(yt,ye)):yds) =
                                                          alphaEq' iis xt yt && alphaEq' iis xe ye &&
                                                          f ((xi,yi):iis) xds yds
                                                        f _ _ _ = False
    (Iletrec xds xe,      Iletrec yds ye)      -> let iis' = zip (map fst xds) (map fst yds) ++ iis
                                                  in  length xds == length yds && alphaEq' iis' xe ye &&
                                                      and (zipWith (\ (xi,(xt,xe)) (yi,(yt,ye)) ->
                                                              alphaEq' iis' xt yt && alphaEq' iis' xe ye)
                                                           xds yds)
    (IWarn _ x,           y)                   -> aeq x y
    (x,                   IWarn _ y)           -> aeq x y
    _                                          -> False
  where aeq = alphaEq' iis

-----

betaEq :: Env -> IExpr -> IExpr -> MT Bool
betaEq r x y = 
    tracex ("betaEq " ++ ppDebug (x,y) ++ (if doTrace2 then ppEnv r else "")) $
    eq r [] x y

type EqPairs = [(IExpr, IExpr)]

hasEqPair :: EqPairs -> IExpr -> IExpr -> Bool
hasEqPair [] _ _ = False
hasEqPair ((x,y):ps) x' y' = 
    alphaEq x x' && alphaEq y y' || alphaEq x y' && alphaEq y x' || hasEqPair ps x' y'

eq :: Env -> EqPairs -> IExpr -> IExpr -> MT Bool
eq r iis x y =
    if x == y || alphaEq x y || hasEqPair iis x y then do
        tracex ("ident eq: " ++ ppDebug (x, y)) $ return ()
        return True
    else 
     let iis' = (x, y) : iis in
     case (x, y) of
      -- XXX these are hacks
--      (Ilet _ _, _) -> do x' <- pushLet x; eq r iis x' y
--      (_, Ilet _ _) -> do y' <- pushLet y; eq r iis x y'
      (IApply _ xf xa, IApply _ yf ya) -> do
          b <- reduceCount 20 (eq r iis' xf yf &&& eq r iis' xa ya)	-- reduce to 20% of the cycles
          if b then return True else eq'' r iis' x y
--          b <- reduceCount 20 (eq r iis' xa ya)	-- reduce to 20% of the cycles
--          if xf == yf && b then return True else eq'' r iis' x y
      _ -> eq'' r iis' x y

eq', eq'' :: Env -> EqPairs -> IExpr -> IExpr -> MT Bool
eq'' r iis x y = do
    n <- getCount
    b <- eq' r iis x y
    n' <- getCount
    tracex ("exit eq:\n"++ ppReadable(x,y)++"is " ++show b) $ return ()
    tracex2 ("eq count=" ++ show (n-n') ++ " is " ++ show b ++ " " ++ ppReadable (x,y) ++ ppEnv r) $ return ()
    return b

eq' r iis x y = do
    step
    x' <- whnf r x >>= caseCommute
    y' <- whnf r y >>= caseCommute
    tracex ("enter eq: "++ ppDebug (x, y) ++ if doFull then ppDebug (x', y') ++ "EQ " ++ show (x' == y') ++ "\n" ++ ppEnv r ++ ppDebug iis else "") $ return ()
    if x' == y' then
        return True
     else
	case tracex2 ("eq: whnf: "++ ppAll ((x,y),(x', y'))) $
	     (x', y') of
	 (IUniv xb xi xe, IUniv yb yi ye) -> return (xb==yb) &&& 
                                             eqFun r iis xi yi xe ye
	 (Ilam  xb xi xe, Ilam  yb yi ye) -> return (xb==yb) &&& 
					     eqFun r iis xi yi xe ye
	 (IProduct xas, IProduct yas) -> return (length xas == length yas) &&&
                                         (zipWithM (\ (xi, (_, xt, xoe)) (yi, (_, yt, yoe)) ->
	 						return (xi==yi) &&& 
							eq r iis (xdef xt) (ydef yt) &&&
							case xoe of
							Nothing -> return (yoe == Nothing)
							Just xe -> 
                                                            case yoe of 
							    Nothing -> return False
							    Just ye -> eq r iis (xdef xe) (ydef ye))
						  xas yas
					 >>= return . and)
                       where xdef e = foldr (Ilam False) e [(i, t) | (_, (i, t, _)) <- orderIProduct xas]
                             ydef e = foldr (Ilam False) e [(i, t) | (_, (i, t, _)) <- orderIProduct yas]
	 (IRecord  xd,  IRecord  yd)  -> return (length xd == length yd) &&&
                                         (zipWithM (\ (xi, (xt,xe,_)) (yi, (yt,ye,_)) ->
	 						return (xi==yi) &&& 
							eq r iis xt yt &&&
							eq r iis xe ye)
						  xd yd
					 >>= return . and)
	 (ISelect xe xi,ISelect ye yi)-> return (xi == yi) &&& eq r iis xe ye
	 (ISum xcs,     ISum ycs)     -> return (length xcs == length ycs) &&&
                                         (zipWithM (\ (xc, xts) (yc, yts) -> 
	 						return (xc==yc) &&& eqCS r iis xts yts)
						  xcs ycs
					 >>= return . and)
	                                 where eqCS _ _ [] [] = return True
					       eqCS r iis ((xi, xt):xits) ((yi, yt):yits) =
					           eq r iis xt yt &&& 
						   eqCS (extendEnv xi (local xt) r) iis xits (mapSnd (iNSubstId1 yi xi) yits)
					       eqCS _ _ _ _ = return False
	 (ICon xc xt,   ICon yc yt)   -> return (xc == yc) &&& eq r iis xt yt
	 (Icase xe xas xd xt, Icase ye yas yd yt) -> 
                return (length xas == length yas) &&&
		eq r iis xe ye &&&
		eq r iis xd yd &&&
		(zipWithM ( \ (xc, xrhs) (yc, yrhs) -> return (xc == yc) &&& eq r iis (patToLam' xrhs) (patToLam' yrhs))
		         (sortFst xas) (sortFst yas) >>= return . and)
--         (Icase _ _ _ _, _) -> eqCase r iis x' y'
--         (_, Icase _ _ _ _) -> eqCase r iis x' y'
	 (IKind xk xl,  IKind yk yl)  -> return (xk == yk && xl == yl)
	 (ILVar xv,     ILVar yv)     -> tracex2 ("eq var " ++ ppDebug (xv,yv,xv==yv)) $
                                         return (xv == yv)
	 (IGVar xv,     IGVar yv)     -> return (xv == yv)
	 (IApply _ xf xa, IApply _ yf ya) -> eq r iis xf yf &&& eq r iis xa ya
	 (Ilet _ _,     Ilet _ _)     -> internalError "eq: Ilet"
	 (Iletrec _ _,  Iletrec _ _)  -> internalError "eq: Iletrec"
         (ILit _ lx,    ILit _ ly)    -> return (lx == ly)
	 (IWarn _ x,    y)            -> eq r iis x y
	 (x,            IWarn _ y)    -> eq r iis x y
	 (_,                   _)     -> return False

eqs :: Env -> EqPairs -> [IExpr] -> [IExpr] -> MT Bool
eqs r iis xes yes = zipWithM (eq r iis) xes yes >>= return . and

eqFun :: Env -> EqPairs -> IArg -> IArg -> IExpr -> IExpr -> MT Bool
eqFun r iis (xi,xt) (yi,yt) xe ye = 
    eq r iis xt yt &&&
    if isDummyUId xi || isDummyUId yi then
        eq r iis xe ye
    else
        eq (extendEnv xi (local xt) r) iis xe (iNSubstId1 yi xi ye)

{-
eqCase r iis x y = do
    x' <- caseCommute x
    y' <- caseCommute y
    traces ("case-commute "++ppAll((x,x'),(y,y'))) $ return ()
    if x' /= x || y /= y' then
        case (x', y') ->
	(Icase xe xas xd xt, Icase ye yas yd yt) -> 
            return (length xas == length yas) &&&
	    eq r iis xe ye &&&
	    eq r iis xd yd &&&
	    (zipWithM ( \ (xc, xrhs) (yc, yrhs) -> return (xc == yc) &&& eq r iis (patToLam' xrhs) (patToLam' yrhs))
	             (sortFst xas) (sortFst yas) >>= return . and)
        _ -> eq r iis x' y'
     else
        return False
-}

caseCommute ec@(Icase e@(Icase ce carms cd ct) arms d t) = do
        let fArm (c, (vs, e)) = do
                ec' <- cloneExpr (Icase e arms d t)
                return (c, (vs, ec'))
        carms' <- mapM fArm carms
        cd' <- cloneExpr (Icase cd arms d t)
        ret <- return (Icase ce carms' cd' t)
        tracex ("case-of-case "++ ppDebug (ec, e, ret)) $ return ()
        return ret
-- XXX This is a hack.
caseCommute (Icase e@(Ilet _ (Icase _ _ _ _)) arms d t) = do
        e' <- pushLet e
	caseCommute (Icase e' arms d t)

caseCommute ec = return ec

(&&&) :: M s Bool -> M s Bool -> M s Bool
(&&&) = liftM2 (&&)

addLetBind b (Ilet bs e) = Ilet (b:bs) e
addLetBind b e = Ilet [b] e

------------------

pushLet :: IExpr -> MT IExpr
pushLet (Ilet    ds e) = push iLetSimpl    ds e
pushLet (Iletrec ds e) = push iLetrecSimpl ds e
pushLet e = return e

-- XX Could use less cloning
push :: ([IDef] -> IExpr -> IExpr) -> [IDef] -> IExpr -> MT IExpr
push f ds (IUniv b (i,t) e) = pushBind f ds (IUniv b) i t e
push f ds (Ilam  b (i,t) e) = pushBind f ds (Ilam  b) i t e
push f ds (IProduct as) = do
    as' <- mapM (\ (i,(ui,t,oe)) -> do
    		   t' <- cloneExpr (f ds t)
                   oe' <- case oe of Nothing -> return Nothing
                                     Just e' -> cloneExpr (f ds e') >>= return . Just
                   return (i, (ui, t', oe'))) as
    return (IProduct as')	-- as' is in canonical order
push f ds (IRecord as) = do
    as' <- mapM (\ (i,(t,e,con)) -> do
    		   t' <- cloneExpr (f ds t)
                   e' <- cloneExpr (f ds e)
                   return (i, (t', e', con))) as
    return (IRecord as')	-- as' is in canonical order
push f ds (ISelect e i) =
    return (ISelect (f ds e) i)
push f ds e@(ISum cs) = do
    ~(ISum cs) <- cloneExpr e
    let cs' = mapSnd (mapSnd (f ds)) cs
    return (ISum cs')
push f ds (ICon i t) = do
    t' <- push f ds t
    return (ICon i t')
push f ds (Icase e as d t) = do
    let e' = f ds e
    as' <- mapM (\ (c, (args, e)) -> cloneExpr (f ds e) >>= \ e' ->
                                   pushCaseArgs f ds args >>= \ args' ->
                                   return (c, (args', e'))) as
    d' <- cloneExpr (f ds d)
    t' <- cloneExpr (f ds t)
    return (Icase e' as' d' t')
push f ds e@(IKind _ _) = return e ---- internalError "push IKind"
push f ds e@(ILVar i) = return e ---- internalError ("push ILVar " ++ ppDebug (Ilet ds (ILVar i)))
push f ds e@(IGVar i) = return e ---- internalError "push IGVar"
push f ds (IApply h g a) = do
    let g' = f ds g
    a' <- cloneExpr (f ds a)
    return (IApply h g' a')
push f ds e@(Ilet _ _) = internalError ("push Ilet\n" ++ ppReadable e)
push f ds e@(Iletrec _ _) = internalError ("push Iletrec\n" ++ ppReadable e)
push f ds (ILit t l) = do
    t' <- push f ds t
    return (ILit t' l)
push f ds (IWarn w e) = do
    e' <- push f ds e
    return (IWarn w e')

pushCaseArgs :: ([IDef] -> IExpr -> IExpr) -> [IDef] -> [(UId, IType)] -> MT [(UId, IType)]
pushCaseArgs f ds args =
  mapM (\ (uid, typ) -> cloneExpr (f ds typ) >>= \ typ' -> return (uid, typ')) args



pushBind :: ([IDef] -> IExpr -> IExpr) -> [IDef] -> 
            ((UId, IType) -> IExpr -> IExpr) -> 
            UId -> IExpr -> IExpr -> MT IExpr
pushBind f ds con i t e = do
    let t' = f ds t
    e' <- cloneExpr (f ds e)
    return (con (i, t') e')

-------------------

-- Find the type of a term when we don't really care about 
-- anything except the outer structure.
typeOfX :: Env -> IExpr -> IType
typeOfX r e =
    case run (TState 1000000000 5000) (typeOf r e) of
    Right t -> t
    Left msg -> internalError ("typeOfX " ++ prEMsg msg ++ ppDebug e)

-- Return the type (in whnf) of a term.
typeOf :: Env -> IExpr -> MT IType
typeOf r ei@(IUniv _ a@(i,t) e) = do
    et <- typeOf (extendEnv i (local t) r) e
    s <- typeOf r t
    let okType (IKind _ _) = True
        okType (IUniv _ _ t) = okType t
	okType _ = False
    if okType et then return (univKind s et) else
     internalError ("typeOf IUniv: " ++ ppDebug (ei, et))
typeOf r (Ilam b a@(i,t) e) = do
    et <- typeOf (extendEnv i (local t) r) e
    cloneExpr (iUniv b a et)
typeOf r (IProduct ss) = 
    do
        let r' = extendEnvs [(i, mdefn t me) | (_, (i, t, me)) <- ss] r
        ts <- mapM (typeOf r') [ t | (_, (_, t, _)) <- ss]
	return (mmKind ts)
typeOf r (IRecord ds) = do
    ss <- mapM (\ (i, (t, e, con)) -> do
                   ui <- mkUniqId i
                   t' <- cloneExpr t
                   oe <- if con then cloneExpr e >>= return . Just else return Nothing
                   return (i, (ui, t', oe))
               ) ds
    return (canonIProduct ss)
typeOf r (ISelect e i) = do
    t' <- typeOf r e
    case t' of
     IProduct as -> case lookup i as of 
                    Just (_, t, _) -> iOpenSimpl e as t >>= whnf r
                    Nothing -> internalError ("typeOf: ISelect lookup " ++ ppReadable(i, as))
     _ -> internalError "typeOf: ISelect"
typeOf r (ISum cs) = 
    do
        let f r [] = return []
	    f r ((i, t) : its) = do
	        k <- typeOf r t
	        ks <- f (extendEnv i (local t) r) its
		return (k : ks)
        tss <- mapM (f r . snd) cs
        return (IKind (maximum (0 : [ n | ts <- tss, IKind n _ <- ts ])) 0)
typeOf r e@(ICon i t@(ISum cs)) = 
    case lookup i cs of
    Just ts -> cloneExpr (foldr (IUniv False) t ts)
    Nothing -> internalError ("typeOf: ICon: " ++ ppDebug e)
typeOf r (ICon _ t) = cloneExpr t -- t is always in WHNF
typeOf r (Icase e _ _ t) = do
    t' <- whnf r t
    case t' of
     IUniv _ (v,vt) tt -> whnfClone r (subst v vt e tt)
     _ -> internalError ("typeOf Icase")
typeOf r (IKind n m) = return (IKind (n+1) (m+1))
typeOf r (ILVar v) = case lookupType v r of 
                         Just t -> whnfClone r t
                         _ -> internalError ("typeOf local not found: " ++ ppDebug v ++ ppEnv r)
typeOf r (IGVar v) = case lookupTypeG v r of 
                         Just t -> whnfClone r t
                         _ -> internalError ("typeOf global not found: " ++ ppDebug v ++ ppEnv r)
typeOf r e@(IApply _ f a) = do
    t' <- typeOf r f
    case t' of
     IUniv _ (i, t) b -> whnfClone r (subst i t a b)
     _ -> internalError ("typeOf: IApply type=" ++ ppDebug t' ++"\ne=" ++ ppDebug e ++ ppEnv r)
typeOf r (Ilet ds e) = do
    t <- typeOf (extendEnvs [(i, (defn t e)) | (i, (t, e)) <- ds] r) e
    whnfClone r (iLetSimpl ds t)
typeOf r (Iletrec ds e) = do
    t <- typeOf (extendEnvs [(i, (defn t e)) | (i, (t, e)) <- ds] r) e
    whnfClone r (iLetrecSimpl ds t)
typeOf r (ILit t _) = whnfClone r t
typeOf r (IWarn _ e) = typeOf r e
typeOf r e = internalError ("no match in typeOf: " ++ ppReadable e)

mmKind [] = iStar
mmKind ts = IKind (maximum [ n | IKind n _ <- ts ]) (minimum [ n | IKind _ n <- ts ])

univKind (IKind m _) (IKind m' n') = IKind (max m m') n'
--univKind (IKind m n) k@(IKind m' n') = k

whnfClone :: Env -> IExpr -> MT IExpr
whnfClone r e = cloneExpr e >>= whnf r

cloneExpr :: IExpr -> MT IExpr
cloneExpr e = clone [] e
  where clone r (IUniv b (i,t) e) =
	        if isDummyUId i then do
		    t' <- clone r t
		    e' <- clone r e
		    return (IUniv b (i, t') e')
		else do
		    i' <- cloneId i
		    t' <- clone r t
		    e' <- clone ((i, ILVar i'):r) e
		    return (IUniv b (i', t') e')
	clone r (Ilam b (i,t) e) = do
		i' <- cloneId i
		t' <- clone r t
		e' <- clone ((i, ILVar i'):r) e
		return (Ilam b (i', t') e')
	clone r (IProduct ss) = do
		let is = [ ui | (_,(ui,_,_)) <- ss]
		is' <- mapM cloneId is
		let r' = zip is (map ILVar is') ++ r
		    f (i, (_, t, me)) ui' = do
		        t' <- clone r' t
			me' <- case me of Nothing -> return Nothing; Just e -> clone r' e >>= return . Just
			return (i, (ui', t', me'))
		ss' <- zipWithM f ss is'
		return (IProduct ss')	-- ss' is in canonical order
	clone r (IRecord ds) = do
		ds' <- mapM (\ (i,(t,e,con)) -> do
				t' <- clone r t
				e' <- clone r e
				return (i,(t',e',con))) ds
		return (IRecord ds')	-- ds' is in canonical order
	clone r (ISelect e i) = do
		e' <- clone r e
		return (ISelect e' i)
	clone r (ISum cs) = do
		cs' <- mapM (\ (c, its) -> do
			        let f r [] = return []
			            f r ((i, t):its) =
			                if isDummyUId i then do
			                    t' <- clone r t
			                    its' <- f r its
			                    return ((i, t') : its')
			                else do
			     		    i' <- cloneId i
					    t' <- clone r t
			     		    its' <- f ((i, ILVar i'):r) its
			                    return ((i', t') : its')
				ts' <- f r its
				return (c, ts')) cs
		return (ISum cs')
	clone r (ICon i t) = do
		t' <- clone r t
		return (ICon i t')
	clone r (Icase e as d t) = do
		e' <- clone r e
		as' <- mapM (apArm (clone r)) as
		d' <- clone r d
		t' <- clone r t
		return (Icase e' as' d' t')
	clone r e@(IKind _ _) = return e
	clone r e@(ILVar i) = return (lookupWithDefault r e i)
	clone r e@(IGVar _) = return e
	clone r (IApply h f a) = do
		f' <- clone r f
		a' <- clone r a
		return (IApply h f' a')
	clone r (Ilet ds e) =
		let f [] r rds = do
			e' <- clone r e
			return (Ilet (reverse rds) e')
		    f ((i,(t,e)):ds) r rds = do
		        i' <- cloneId i
		        t' <- clone r t
			e' <- clone r e
			f ds ((i, ILVar i'):r) ((i',(t',e')):rds)
		in  f ds r []
	clone r (Iletrec ds e) = do
		let is = map fst ds
		is' <- mapM cloneId is
		let r' = zip is (map ILVar is') ++ r
		ds' <- zipWithM (\ (i,(t,e)) i' -> do
				t' <- clone r' t
				e' <- clone r' e
				return (i',(t',e')) ) ds is'
		e' <- clone r' e
		return (Iletrec ds' e')
	clone r (ILit t l) = do
	        t' <- clone r t
		return (ILit t' l)
	clone r (IWarn w e) = do
		e' <- clone r e
		return (IWarn w e')

apArm f a@(c, (vs, _)) = 
    do e' <- f (patToLam a)
       return (c, getNLams (length vs) [] e')

iOpenSimpl :: IExpr -> [ISign] -> IType -> MT IType
iOpenSimpl e ss t = do
    (def, var) <- 
        case e of 
        ILVar _ -> return ([], e)
        IGVar _ -> return ([], e)
        _ -> do
             v <- makeVar Nothing
             return ([(v, (canonIProduct ss, e))], ILVar v)
    let ds = [ (ui, (t, ISelect var i)) | ( i, (ui, t, _)) <- ss ]
    cloneExpr (iLetrecSCC (def ++ ds) t)

-----------------

primBinOp r (ILVar v) =
    case lookupValue v r of
    Just e -> primBinOp' e
    Nothing -> Nothing
primBinOp _ e = primBinOp' e

primBinOp' (ISelect (IGVar si) fun) | si == integerModId =
    case lookup (getIdString fun) integerBinOps of
    Just op -> Just $
    	\ e1 -> \ e2 ->
        case (e1, e2) of
           (ILit t (LInteger i1), ILit _ (LInteger i2)) -> Just (ILit t (LInteger (op i1 i2)))
           _ -> Nothing
    Nothing -> Nothing
primBinOp' (ISelect (IGVar si) fun) | si == intModId =
    case lookup (getIdString fun) integerBinOps of
    Just op -> Just $
    	\ e1 -> \ e2 ->
        case (e1, e2) of
           (ILit t (LInt i1), ILit _ (LInt i2)) -> Just (ILit t (LInt (op i1 i2)))
           _ -> Nothing
    Nothing -> Nothing
primBinOp' _ = Nothing

integerBinOps :: [(String, Integer -> Integer -> Integer)]
integerBinOps = [
    ("+", (+)),
    ("-", (-)),
    ("*", (*)),
    ("quot", quot),
    ("rem", rem)
    ]

notNative (LNative _) = False
notNative _ = True
