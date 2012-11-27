module IUtil(iFreeVars, iFreeVarsL, iNSubst, iNSubstId, iNSubstId1, patToLam, getNLams, patToLam',
	iUniv, iLam, iGVars, iLetSimpl, iLetrecSimpl, iLetrecSCC, iCollectDefRec,
        isKindRes, isKind, iBoundVars, canonIProduct, canonIRecord, orderIProduct,
	isModuleId, isModuleString, spine, subst) where
import Data.List(union, (\\), intersect, partition)
import Data.Maybe(maybeToList)
import ListUtil(mapSnd, mapFst, lookupWithDefault)
import SCC
import Error(internalError)
import Position
import Id
import ISyntax
import Util(unions, assoc, sortFst)
import qualified UIdSet as S

--import ISyntaxPP
--import PPrint
--import Util(traces)

iFreeVars :: IExpr -> S.UIdSet
iFreeVars (IUniv _ (i, t) e) = iFreeVars t `S.union` (S.delete i (iFreeVars e))
iFreeVars (Ilam  _ (i, t) e) = iFreeVars t `S.union` (S.delete i (iFreeVars e))
iFreeVars (IProduct its) = 
    S.unions (map (\ (_, (_, t, oe)) -> iFreeVars t `S.union` S.unions (map iFreeVars (maybeToList oe))) its)
    `S.minus` S.fromList [ i | (_,(i,_,_)) <- its]
iFreeVars (IRecord ds) = 
    S.unions (map (\ (_,(t,e,_)) -> iFreeVars t `S.union` iFreeVars e) ds)
iFreeVars (ISelect e _) = iFreeVars e
iFreeVars (ISum cs) = S.unions (map (iFreeCS . snd) cs)
  where iFreeCS its = foldr (\ (i, t) vs -> S.union (iFreeVars t) (S.delete i vs)) S.empty its
iFreeVars (ICon _ t) = iFreeVars t
iFreeVars (Icase e cs d t) = 
    S.unions (iFreeVars e : iFreeVars d : iFreeVars t : map (iFreeVars . patToLam) cs)
iFreeVars (IKind _ _) = S.empty
iFreeVars (ILVar v) = S.singleton v
iFreeVars (IGVar v) = S.empty
iFreeVars (IApply _ f a) = iFreeVars f `S.union` iFreeVars a
iFreeVars (Ilet [] e) = iFreeVars e
iFreeVars (Ilet ((i,(t,e)):ds) b) = 
    (iFreeVars t `S.union` iFreeVars e) `S.union` (S.delete i (iFreeVars (Ilet ds b)))
iFreeVars (Iletrec ds e) = 
    (S.unions [iFreeVars e `S.union` iFreeVars t | (_,(t,e)) <- ds] `S.union` iFreeVars e) `S.minus`
    S.fromList (map fst ds)
iFreeVars (ILit t _) = iFreeVars t
iFreeVars (IWarn _ e) = iFreeVars e
iFreeVars (IHasType e t) = iFreeVars e `S.union` iFreeVars t

iFreeVarsL :: IExpr -> [UId]
iFreeVarsL (IUniv _ (i, t) e) = iFreeVarsL t `union` (iFreeVarsL e \\ [i])
iFreeVarsL (Ilam  _ (i, t) e) = iFreeVarsL t `union` (iFreeVarsL e \\ [i])
iFreeVarsL (IProduct its) = 
    unions (map (\ (_, (_, t, oe)) -> iFreeVarsL t `union` unions (map iFreeVarsL (maybeToList oe))) its)
    \\ [ i | (_,(i,_,_)) <- its]
iFreeVarsL (IRecord ds) = 
    unions (map (\ (_,(t,e,_)) -> iFreeVarsL t `union` iFreeVarsL e) ds)
iFreeVarsL (ISelect e _) = iFreeVarsL e
iFreeVarsL (ISum cs) = unions (map (iFreeCSL . snd) cs)
  where iFreeCSL its = foldr (\ (i, t) vs -> union (iFreeVarsL t) (vs \\ [i])) [] its
iFreeVarsL (ICon _ t) = iFreeVarsL t
iFreeVarsL (Icase e cs d t) = 
    unions (iFreeVarsL e : iFreeVarsL d : iFreeVarsL t : map (iFreeVarsL . patToLam) cs)
iFreeVarsL (IKind _ _) = []
iFreeVarsL (ILVar v) = [v]
iFreeVarsL (IGVar v) = []
iFreeVarsL (IApply _ f a) = iFreeVarsL f `union` iFreeVarsL a
iFreeVarsL (Ilet [] e) = iFreeVarsL e
iFreeVarsL (Ilet ((i,(t,e)):ds) b) = 
    (iFreeVarsL t `union` iFreeVarsL e) `union` (iFreeVarsL (Ilet ds b) \\ [i])
iFreeVarsL (Iletrec ds e) = 
    (unions [iFreeVarsL e `union` iFreeVarsL t | (_,(t,e)) <- ds] `union` iFreeVarsL e)
    \\ map fst ds
iFreeVarsL (ILit t _) = iFreeVarsL t
iFreeVarsL (IWarn _ e) = iFreeVarsL e
iFreeVarsL (IHasType e t) = iFreeVarsL e `union` iFreeVarsL t

sumMap :: (a -> Int) -> [a] -> Int
sumMap f xs = sum (map f xs)

countOcc :: UId -> IExpr -> Int
countOcc v (IUniv _ (i, t) e) = countOcc v t + 2 * countOcc v e
countOcc v (Ilam  _ (i, t) e) = countOcc v t + 2 * countOcc v e
countOcc v (IProduct its) = 
    -- Normally an identifier only has one binding site, but during type checking
    -- the binding inside a product may be duplicated outside.
    if v `elem` [ i | (_,(i,_,_)) <- its] then
        0
    else
        sumMap (\ (_, (i, t, oe)) -> countOcc v t + sumMap (countOcc v) (maybeToList oe)) its
countOcc v (IRecord ds) =
    sumMap (\ (_, (t,e,_)) -> countOcc v t + countOcc v e) ds
countOcc v (ISelect e _) = countOcc v e
countOcc v (ISum cs) = sumMap (\ (c, its)-> sumMap (countOcc v . snd) its) cs
countOcc v (ICon _ t) = countOcc v t
countOcc v (Icase e cs d t) = sum (countOcc v e : countOcc v d : countOcc v t : map (countOcc v . patToLam) cs)
countOcc v (IKind _ _) = 0
countOcc v (ILVar v') = if v==v' then 1 else 0
countOcc v (IGVar _) = 0
countOcc v (IApply _ f a) = countOcc v f + countOcc v a
countOcc v (Ilet [] e) = countOcc v e
countOcc v (Ilet ((i,(t,e)):ds) b) = 
    countOcc v t + countOcc v e + countOcc v (Ilet ds b)
countOcc v (Iletrec ds e) = 
    sum [countOcc v e + countOcc v t | (_,(t,e)) <- ds] + countOcc v e
countOcc v (ILit t _) = countOcc v t
countOcc v (IWarn _ e) = countOcc v e
countOcc v (IHasType e t) = countOcc v e + countOcc v t

iBoundVars :: IExpr -> [UId]
iBoundVars (IUniv _ (i, t) e) = i : iBoundVars t ++ iBoundVars e
iBoundVars (Ilam  _ (i, t) e) = i : iBoundVars t ++ iBoundVars e
iBoundVars (IProduct its) = 
    concat (map (\ (_, (i, t, oe)) -> i : iBoundVars t ++ concat (map iBoundVars (maybeToList oe))) its)
iBoundVars (IRecord ds) = 
    concat (map (\ (_,(t,e,_)) -> iBoundVars t ++ iBoundVars e) ds)
iBoundVars (ISelect e _) = iBoundVars e
iBoundVars (ISum cs) = concatMap (iBoundVarsCS . snd) cs
  where iBoundVarsCS its = foldr (\ (i, t) vs -> i : iBoundVars t ++ vs) [] its
iBoundVars (ICon _ t) = iBoundVars t
iBoundVars (Icase e cs d t) = 
    concat (iBoundVars e : iBoundVars d : iBoundVars t : map (iBoundVars . patToLam) cs)
iBoundVars (IKind _ _) = []
iBoundVars (ILVar v) = []
iBoundVars (IGVar v) = []
iBoundVars (IApply _ f a) = iBoundVars f ++ iBoundVars a
iBoundVars (Ilet ds e) = 
    concat [i : iBoundVars e ++ iBoundVars t | (i,(t,e)) <- ds] ++ iBoundVars e
iBoundVars (Iletrec ds e) = 
    concat [i : iBoundVars e ++ iBoundVars t | (i,(t,e)) <- ds] ++ iBoundVars e
iBoundVars (ILit t _) = iBoundVars t
iBoundVars (IWarn _ e) = iBoundVars e
iBoundVars (IHasType e t) = iBoundVars e ++ iBoundVars t

iGVars :: IExpr -> [Id]
iGVars (IUniv _ (i, t) e) = iGVars t `union` iGVars e
iGVars (Ilam  _ (i, t) e) = iGVars t `union` iGVars e
iGVars (IProduct its) =
    unions (map (\ (_, (_, t, oe)) -> iGVars t `union` unions (map iGVars (maybeToList oe))) its)
iGVars (IRecord ds) = 
    unions (map (\ (_,(t,e,_)) -> iGVars t `union` iGVars e) ds)
iGVars (ISelect e _) = iGVars e
iGVars (ISum cs) = unions (map (\(c, its) -> unions (map (iGVars . snd) its)) cs)
iGVars (ICon _ t) = iGVars t
iGVars (Icase e cs d t) = 
    unions (iGVars e : iGVars d : iGVars t : [iGVars t | (ICLit t _, _) <- cs ] ++ map (iGVars . patToLam) cs)
iGVars (IKind _ _) = []
iGVars (ILVar v) = []
iGVars (IGVar v) = [v]
iGVars (IApply _ f a) = iGVars f `union` iGVars a
iGVars (Ilet [] e) = iGVars e
iGVars (Ilet ((i,(t,e)):ds) b) = 
    (iGVars t `union` iGVars e) `union` iGVars (Ilet ds b)
iGVars (Iletrec ds e) = 
    unions [iGVars e `union` iGVars t | (_,(t,e)) <- ds] `union` iGVars e
iGVars (ILit t _) = iGVars t
iGVars (IWarn _ e) = iGVars e
iGVars (IHasType e t) = iGVars e `union` iGVars t

-- Naive substitution
iNSubst :: [(UId, IExpr)] -> IExpr -> IExpr
iNSubst [] e = e
iNSubst s (IUniv b (i,t) e) =
    if i `elem` map fst s then internalError ("iNSubst-1" {- ++ppDebug (IUniv b (i,t) e, s)-}) else
    IUniv b (i, iNSubst s t) (iNSubst s e)
iNSubst s (Ilam  b (i,t) e) = 
    if i `elem` map fst s then internalError "iNSubst-2" else
    Ilam  b (i, iNSubst s t) (iNSubst s e)
iNSubst s e@(IProduct its) =
    if not (null (intersect [ i | (_,(i,_,_)) <- its] (map fst s))) then internalError "iNSubst-5" else
    canonIProduct (mapSnd (\ (i, t, oe) -> (i, iNSubst s t, fmap (iNSubst s) oe)) its)
iNSubst s e@(IRecord ds) =
--    if not (null (intersect (map fst ds) (map fst s))) then internalError "iNSubst-6" else
    IRecord [(i, (iNSubst s t, iNSubst s e,con)) | (i,(t,e,con)) <- ds] -- already canonical
iNSubst s (ISelect e i) = ISelect (iNSubst s e) i
iNSubst s (ISum cs) = 
    ISum (mapSnd (mapSnd (iNSubst s)) cs)
iNSubst s (ICon c t) = 
    ICon c (iNSubst s t)
iNSubst s (Icase e as d t) = 
    Icase (iNSubst s e) (map f as) (iNSubst s d) (iNSubst s t)
  where f a@(c, (vs, _)) = (c, getNLams (length vs) [] (iNSubst s (patToLam a)))
iNSubst s e@(IKind _ _) = e
iNSubst s e@(ILVar v) = lookupWithDefault s e v
iNSubst s e@(IGVar _) = e
iNSubst s (IApply h f a) = IApply h (iNSubst s f) (iNSubst s a)
iNSubst s (Ilet    ds e) = 
    if not (null (intersect (map fst ds) (map fst s))) then internalError "iNSubst-3" else
    Ilet    [(i, (iNSubst s t, iNSubst s e)) | (i, (t, e)) <- ds] (iNSubst s e)
iNSubst s (Iletrec ds e) =
    if not (null (intersect (map fst ds) (map fst s))) then internalError "iNSubst-4" else
    Iletrec [(i, (iNSubst s t, iNSubst s e)) | (i, (t, e)) <- ds] (iNSubst s e)
iNSubst s (ILit t l) = ILit (iNSubst s t) l
iNSubst s (IWarn w e) = IWarn w (iNSubst s e)
iNSubst s (IHasType e t) = IHasType (iNSubst s e) (iNSubst s t)

iNSubstId :: [(UId, UId)] -> IExpr -> IExpr
iNSubstId s e = iNSubst (mapSnd ILVar s) e

iNSubstId1 :: UId -> UId -> IExpr -> IExpr
iNSubstId1 i n e = if isDummyUId i then e else iNSubstId [(i,n)] e

iLetrecSCC :: [IDef] -> IExpr -> IExpr
iLetrecSCC ds b =
    let is = map fst ds
        g = [(i, (iFreeVarsL t `union` iFreeVarsL e) `intersect` is) |
             (i, (t, e)) <- ds ]
        iss = scc g
        getDef i = (i, assoc ds i)
        dss = map (map getDef) iss
        def ds b = if isRec ds then iLetrecSimpl ds b else iLetSimpl ds b
        isRec [(i,(t,e))] = i `S.elem` iFreeVars t || i `S.elem` iFreeVars e
        isRec _ = True
    in  foldr def b dss

iLetrecSimpl :: [IDef] -> IExpr -> IExpr
iLetrecSimpl ds b =
    if S.null (iFreeVars b `S.intersect` S.fromList (map fst ds)) then
        b
    else
        Iletrec ds b

iUniv :: Bool -> IArg -> IExpr -> IExpr
iUniv b (i,t) e = 
    if i `S.elem` iFreeVars e then IUniv b (i, t) e 
    else IUniv b (dummyUId (getUIdPosition i), t) e

iLam :: Bool -> IArg -> IExpr -> IExpr
iLam b (i,t) e = 
    if i `S.elem` iFreeVars e then Ilam b (i, t) e 
    else Ilam b (dummyUId (getUIdPosition i), t) e

doTrivBind :: [IDef] -> IExpr -> IExpr
doTrivBind ds e = f (reverse ds) [] e
  where f [] [] e = e
        f [] r  e = Ilet r e
	f (d@(i,(t,e)):ds) r b =
	    case countOcc i (Ilet r b) of
	    0 -> f ds r b
	    1 -> f' i e ds r b
	    _ -> case e of
		 ILVar i' -> 
		     if i == i' then internalError ("doTrivBind") else
		     f' i e ds r b
		 -- Move type selections in
		 ISelect (IGVar _) _ | isKindRes t ->
		     f' i e ds r b
		 _ -> f ds (d:r) b
		 -- XXX Should more selections be moved in?  E.g. selections in System
	f' i e ds r b = let s = [(i,e)] in f ds [(i, (iNSubst s t, iNSubst s e)) | (i, (e, t)) <- r] (iNSubst s b)

iLetSimpl :: [IDef] -> IExpr -> IExpr
iLetSimpl ds e = doTrivBind ds e

isKindRes :: IType -> Bool
isKindRes (IKind _ _) = True
isKindRes (IUniv _ _ r) = isKindRes r
isKindRes _ = False

isKind :: IType -> Bool
isKind (IKind _ _) = True
isKind _ = False

spine :: [IExpr] -> IExpr -> (IExpr, [IExpr])
spine es (IApply _ f e) = spine (e:es) f
spine es f = (f, es)

subst :: UId -> IType -> IExpr -> IExpr -> IExpr
subst x _  _ tb | isDummyUId x = tb
subst x ta a tb = iLetSimpl [(x,(ta,a))] tb

patToLam :: ICaseArm -> IExpr
patToLam (c, (as, e)) = foldr (Ilam False) e as

patToLam' :: ([IArg], IExpr) -> IExpr
patToLam' (as, e) = foldr (Ilam False) e as

getNLams :: Int -> [(UId,IType)] -> IExpr -> ([(UId,IType)], IExpr)
getNLams 0 as e = (reverse as, e)
getNLams n as (Ilam _ a e) = getNLams (n-1) (a:as) e

iCollect :: (IExpr -> [a]) -> IExpr -> [a]
iCollect f e = f e ++
    case e of
    IUniv _ (_,t) e -> iCollect f t ++ iCollect f e
    Ilam  _ (_,t) e -> iCollect f t ++ iCollect f e
    IProduct ss -> concatMap (iCollect f) ([ t | (_, (_, t, _)) <- ss] ++ [ e | (_, (_, _, Just e)) <- ss ])
    IRecord ds -> concatMap (iCollectDefRec f) ds
    ISelect e _ -> iCollect f e
    ISum cs -> concatMap (iCollect f) [ t | (i, its) <- cs, (_, t) <- its ]
    ICon _ t -> iCollect f t
    Icase e as d t -> iCollect f e ++ concatMap (iCollect f . patToLam) as  ++ iCollect f d ++ iCollect f t
    IApply _ e e' -> iCollect f e ++ iCollect f e'
    Ilet ds e -> concatMap (iCollectDef f) ds ++ iCollect f e
    Iletrec ds e -> concatMap (iCollectDef f) ds ++ iCollect f e
    ILit t _ -> iCollect f t
    IWarn _ e -> iCollect f e
    IHasType e e' -> iCollect f e ++ iCollect f e'
    _ -> []
iCollectDef :: (IExpr -> [a]) -> IDef -> [a]
iCollectDef f (_,(t,e)) = iCollect f t ++ iCollect f e
iCollectDefRec :: (IExpr -> [a]) -> IDefRec -> [a]
iCollectDefRec f (_,(t,e,_)) = iCollect f t ++ iCollect f e

----
canonIRecord :: [IDefRec] -> IExpr
canonIRecord ss = IRecord (sortFst ss)

canonIProduct :: [ISign] -> IExpr
canonIProduct ss = IProduct (sortFst ss)

-- Order a product type in canonical order.  The order is according
-- to the dependency in the type part.  The order is crucial for
-- comparison to work!
-- XX Not very efficient
orderIProduct :: [ISign] -> [ISign]
orderIProduct ss =
	let uis = [ ui | (_, (ui, _, _)) <- ss ]
            g0 = [(iFreeVarsL t `intersect` uis, s) |
		  s@(_, (ui, t, _)) <- ss ]
	    step g =
	        case partition (null . fst) g of
		([], []) -> []
		([], _) -> internalError ("canonIProduct ") -- ++ ppDebug ss)
		(ds, g') ->
		    let is = [ ui | (_, (_, (ui, _, _))) <- ds ]
		        g'' = mapFst (\\ is) g'
		    in  sortFst (map snd ds) ++ step g''
	in  step g0
