module TypeCheck(typeCheck, typeCheckInterfaces, typeCheckInterfacesI) where
import Data.List((\\), union, intersect, transpose)
import Control.Monad(zipWithM, unless, when, foldM)
import Libs.ListUtil(mapSnd, lookupWithDefault)
import SCC
import FString
import PreStrings
import Position(noPosition, Position, remPositionFile)
import Id
import MiscId
import Error
import PPrint
import CSyntax
import ISyntax
import ISyntaxPP
import IReduce
import CUtil
import IUtil
import qualified UIdSet as S
import Util(sortGroup, allSame)
import TrivRed
import Env
import TMonad
import CITranslate(iToC)
import FindDiff
import Util(tracex, tracex2, traces, findDup, assoc, doTrace, doFull)

-- XXX
import Libs.IOUtil(progArgs)
haskellFlag = "-haskell" `elem` progArgs

type StructType = Env -> Id -> Maybe IType
noStructType r i = Nothing

typeCheck :: Int -> Env -> CModule -> Either EMsg IModule
typeCheck rsteps r (CModule _ def@(CDef ps d)) = run (TState 10000 rsteps) $ do -- start id, # of reduction steps
    let (_, con) = getPubCon [] def
    n <- getCount
    (me', t') <- tchkDefn r noStructType d
    n' <- getCount
    tracex ("total count="++show (n-n')) $ return ()
    let Just e' = me'
    return (IModule (cDefId def, (t', e', con)))

typeCheckInterfaces :: Int -> [CInterface] -> Either EMsg Env
typeCheckInterfaces rsteps cis = run (TState 1000 rsteps) $
    let ti r [] = return r
	ti r (CInterface _ (CSign i t):is) = do
	    (t', _) <- tchk r t Nothing
            ti (extendEnvG i (local t') r) is
	ti r (CInterface _ (CSignDef d):is) = do
	    (me, t) <- tchkDefn r noStructType d
            ti (extendEnvG (cDefnId d) (case me of Nothing -> local t; Just e -> defn t e) r) is
	ti r (CInterface _ (CSignType i as):is) = do
	    (t', _) <- tchk r (signType as) Nothing
            ti (extendEnvG i (local t') r) is
    in  ti initEnv cis

typeCheckInterfacesI :: Int -> [IInterface] -> Either EMsg Env
typeCheckInterfacesI rsteps iis = run (TState 1000 rsteps) $
    let ti r [] = return r
	ti r (IInterface _ (i, (_, t, me)) : is) = do
	    t' <- cloneExpr t
	    me' <- case me of Nothing -> return Nothing; Just e -> cloneExpr e >>= return . Just
	    ti (extendEnvG i (mdefn t' me') r) is
    in  ti initEnv iis

chkT :: Env -> CExpr -> Maybe IType -> IType -> MT IType
chkT _ _ Nothing t = return t
chkT r e (Just t) t' = do
    b <- betaEqT r t t'
    unless b $
        typeError2 r "chkT" "" e t' t
    return t

tchk, tchk' :: Env -> CExpr -> Maybe IType -> MT (IExpr, IType)
tchk r e mt = do
    tracex ("enter tchk: " ++ ppDebug e ++ "want type: " ++ ppDebug mt) $ return ()
    et@(e',t') <- tchk' r e mt
    tracex ("exit tchk: " ++ ppDebug e' ++ ppDebug t' ++ "----\n") $ return ()
    return et

tchk' r e@(CUniv (bb,a) tb) mt = do
    (i, ta, s) <- tchkArg r a Nothing
    s' <- whnfT e r s
    (tb', t) <- tchkRed (extendEnv i (local ta) r) tb Nothing
    rt <- chkT r e mt (univKind s' t)
    unless (okKindOf s' t) $
        kindError r (getExprPosition e) ta s' t
    return (IUniv bb (i, ta) tb', rt)
tchk' r e@(CArrow b ta tb) mt = do
    (ta', s) <- tchkRed r ta Nothing
    (tb', t) <- tchkRed r tb Nothing
    rt <- chkT r e mt (univKind s t)
    unless (okKindOf s t) $
        kindError r (getExprPosition e) ta s t
    return (IUniv b (dummyUId (getExprPosition ta), ta') tb', rt)
tchk' r e@(Clam (bb,a) b) mt =
    case mt of
    Nothing -> do
        (i, ta, s) <- tchkArg r a Nothing
	(b', tb) <- tchk (extendEnv i (local ta) r) b Nothing
        ta' <- cloneExpr ta
	t <- mkUniv bb i ta' tb
	return (Ilam bb (i, ta) b', t)
    Just t -> do
        t' <- whnfI r t
	case t' of
            (IUniv bb' (si, sa) sb) | bb==bb' -> do
                (i, ta, s) <- tchkArg r a (Just sa)
		(b', tb) <- tchk (extendEnv i (local ta) r) b (Just (iNSubstId1 si i sb))
		return (Ilam bb (i, ta) b', t)
	    _ ->
	        typeError2 r "lam" "" e iFunc t
tchk' r (Clet ds b) mt = tchkRec (\r->tchk r b mt) r noStructType [Cconcrete] ds
tchk' r ep@(CProduct _ []) (Just t) = do
    t' <- whnfI r t
    case t' of
        IKind _ _ -> return (IProduct [], t')
        _ -> typeError2 r "Product" "" ep t' iStar
tchk' r ep@(CProduct _ ss0) mt =
    let ss = concatMap (\s->
    		case s of
		CSignDef d -> s : [ CSignDef d | CDef _ d <- mkConBinds [] (CDef [] d)]
		_ -> [s]
		) ss0
        ds = map (\ s ->
    		case s of
		CSign i t -> CDef [] (CDSign i t)
		CSignDef d -> CDef [] d
		CSignType i as -> CDef [] (CDSign i (signType as))
		) ss
	mkBody r = do
	    let f (CSign i _) =
	            case lookupUId i r of
		    Just (ui, t) -> (i, (ui, t, Nothing))
		    _ -> undefined
		f (CSignDef d) =
		    let i = cDefnId d in
	            case lookupUId i r of
		    Just (ui, t) -> (i, (ui, t, lookupValue ui r))
		    _ -> undefined
	        f (CSignType i _) =
	            case lookupUId i r of
		    Just (ui, t) -> (i, (ui, t, Nothing))
		    _ -> undefined
		ss' = map f ss
	        er = canonIProduct ss'
	    rt <- typeOf r er
	    rt' <- chkT r ep mt rt
	    return (er, rt')
    in  tchkRec mkBody r noStructType [Cpublic, Cconcrete] ds
tchk' r er@(CRecord ps _ ds0) mt = do
    (as, iis) <- case mt of
	         Nothing -> return ([], [])
		 Just t -> do
	             t' <- whnfI r t
		     case t' of
		      IProduct its -> return ([(i, t)  | (i, (_,  t, _)) <- its],
					      [(ui, i) | (i, (ui, _, _)) <- its])
		      _ -> return ([], [])
    let expandOpen (CDef ps (CDopen e (COpenArgs as))) = return $ map (openArg ps e) as
	expandOpen (CDef ps (CDopen e COpenAll)) = do
	    (_, t) <- tchkRed r e Nothing
	    case t of
	     IProduct ss -> return $ map ((\ i -> openArg ps e $ COpenArg [] i Nothing Nothing) . fst) ss
	     _ -> errorAtExpr e (ENotProductOpen (ppReadable t))
        expandOpen d = return [d]
        openArg ps e (COpenArg ps' i ot oi) = 
            let nps = [if isProp Cpublic   ps ps' False then Cpublic   else Cprivate,
                       if isProp Cconcrete ps ps' False then Cconcrete else Cabstract]
                i' = case oi of
                     Nothing -> i
                     Just i -> i
                d = case ot of 
                    Nothing -> CValue i (CSelect e i')
                    Just t -> CValueT i [] t (CSelect e i')
            in  CDef nps d
    ds <- concatMapM expandOpen ds0
    let stf r i =
	    case lookup i as of
	    Nothing -> Nothing
	    Just t -> let vs = iFreeVars t
			  iis' = filter ((`S.elem` vs) . fst) iis
			  s = [ (ui, n) | (ui, i) <- iis', Just (n, _) <- [lookupUId i r] ]
	              in  if length iis' == length s then Just (iNSubstId s t) else Nothing
	mkBody r = do
	    -- If we have a type signature, try to impose it on the struct value
	    -- by making non-existant identifiers private.
	    vis <-
	            case mt of
		    Nothing -> return (\ i -> True)
		    Just t -> do t' <- whnf r t
		                 case t' of
				  IProduct ss -> let is = map fst ss in return (`elem` is)
				  _ -> return (\ i -> True)
            let mkD d =
                    case getPubCon ps d of
                    (True, con) | vis i -> 
                        case lookupUId i r of
                        Just (ui, t) -> do
                            t' <- cloneExpr t
                            return [(i, (t', ILVar ui, con))]
                        _ -> internalError "tchk: CRecord lookupUId"
		       where i = cDefId d
                    _ -> return []
            dss <- mapM mkD (ds ++ concatMap (mkConBinds ps) ds)
            let ds' = concat dss
                getVal ui =
                    case lookupValue ui r of
                    Just e -> Just e
                    _ -> internalError "tchk: CRecord lookupValue"
                -- it's ok to reuse expressions in ss since tchkRec will clone it
		ss = [(i, (ui, t, if con then getVal ui else Nothing)) |
                      (i, (t, ILVar ui, con)) <- ds']
		rt = canonIProduct ss
	    rt' <- chkT r er mt rt
	    return (canonIRecord ds', rt')
    tchkRec mkBody r stf ps ds
tchk' r0 eo@(Copen e oas b) mt = do
    (e', t') <- tchkRed r0 e Nothing
    (def, var, r) <- 
        case e' of 
        ILVar _ -> return ([], e', r0)
        IGVar _ -> return ([], e', r0)
        _ -> do
             v <- makeVar Nothing
             return ([(v, (t', e'))], ILVar v, extendEnv v (defn t' e') r0)
    case t' of
     IProduct ss -> do
        let as = case oas of COpenArgs as -> as; COpenAll -> map ((\ i -> COpenArg [] i Nothing Nothing) . fst) ss
        uis <- mapM (mkUniqId . (\ (COpenArg _ i _ _) -> i)) as
        let f :: COpenArg -> Env -> MT (IType, IExpr)
	    f (COpenArg _ i Nothing oi) _ =
	        case lookup i ss of
		Just (_, t, me) -> do
		    body <- doInline e' ss me (ISelect var (h i oi))
		    t' <- iOpenSimpl e' ss t	-- iOpenSimpl does the cloning
		    return (t', body)
		Nothing -> errorAtId i (ENotProductElem (ppReadable t'))
	    f (COpenArg _ i (Just ct) oi) rr =
	        case lookup i ss of
		Just (_, t, me) -> do
		    body <- doInline e' ss me (ISelect var (h i oi))
		    t' <- iOpenSimpl e' ss t	-- iOpenSimpl does the cloning
		    (ct', _) <- tchk rr ct Nothing
                    t'' <- chkT rr (CVar i) (Just t') ct'
		    return (t'', body)
		Nothing -> errorAtId i (ENotProductElem (ppReadable t'))
            h i (Just i') = i'
            h i Nothing = i
            g :: ([(UId, (IType, IExpr))], Env) -> (COpenArg, UId) -> MT ([(UId, (IType, IExpr))], Env)
            g (ds, r) (a, ui) = do
                te@(t, e) <- f a r
                return ((ui, te):ds, extendEnv ui (defn t e) r)
        (ds, r') <- foldM g ([], r) (zip as uis)
	(b', bt) <- tchk r' b mt
	let ds' = def ++ ds
	bt' <- cloneExpr (iLetrecSCC ds' bt)
        return (iLetrecSCC ds' b', bt')
     _ -> errorAtExpr eo (ENotProductOpen (ppReadable t'))
tchk' r es@(CSelect e i) mt = do
    (e', t') <- tchkRed r e Nothing
    case t' of
     IProduct as ->
        case lookup i as of
	Just (_, t, me) -> do
	    body <- doInline e' as me (ISelect e' i)
            t' <- iOpenSimpl e' as t	-- iOpenSimpl does the cloning
	    rt' <- chkT r es mt t'
	    return (body, rt')
	Nothing -> errorAtId i (ENotProductElem (ppReadable t'))
     _ -> errorAtId i (ENotProduct (ppReadable t'))
tchk' r es@(CSum cs) mt = do
    case findDup (map fst cs) of
     i:_ -> errorAtId i EDuplicateCon
     [] -> do
         let f (c, its) = do
	         let g _ [] = return []
		     g r (CSType t : its) = do
	                 (t', k) <- tchkRed r t Nothing
			 if isKind k then do -- XXX level
			     its' <- g r its
			     return ((dummyUId noPosition, t') : its')
			  else
			     errorAtId c (EConNonKind (ppReadable t))
		     g r (CSId i t : its) = do
		         (i', t', k) <- tchkArg r (CArgT i t) Nothing
			 if isKind k then do -- XXX level
			     its' <- g (extendEnv i' (local t') r) its
			     return ((i', t') : its')
			  else
			     errorAtId c (EConNonKind (ppReadable t))
	         ts' <- g r its
	         return (c, ts')
         cs' <- mapM f cs      -- XXX SORT
	 rt <- typeOf r (ISum cs')
	 rt' <- chkT r es mt rt
	 return (ISum cs', rt')
tchk' r ec@(CCon i (CApply (CVar u) [])) mt | isDummyId u =
  case mt of
  Nothing -> errorAtExpr ec EConUnknown
  Just t' -> do
    t'' <- whnfI r t'
    case t'' of
     ISum cs ->
        case lookup i cs of
	Just its -> do
	    let rt = foldr (IUniv False) t' its
	    rt' <- chkT r ec mt rt
	    return (ICon i t'', rt')
	Nothing -> errorAtId i (ENotSumElem (ppReadable t''))
     t -> errorAtId i (ENotSum (ppReadable t))
tchk' r (CApply ec@(CCon i (CApply (CVar u) [])) es) mt | isDummyId u =
  case mt of
  Nothing -> errorAtExpr ec EConUnknown
  Just t' -> do
    t'' <- whnfI r t'
    case t'' of
     ISum cs ->
        case lookup i cs of
	Just _ -> tchk' r (CApply (CCon i (iToC False t')) es) mt
{-
          do
	    let rt = foldr iArrow t' ts
	    rt' <- chkT r ec mt rt
	    return (ICon i t'', rt')
-}
	Nothing -> errorAtId i (ENotSumElem (ppReadable t''))
     t -> errorAtId i (ENotSum (ppReadable t))
tchk' r ec@(CCon i t) mt = do
    (t', _) <- tchk r t Nothing
    t'' <- whnfT t r t'
    case t'' of
     ISum cs ->
        case lookup i cs of
	Just ts -> do
	    let rt = foldr (IUniv False) t' ts
	    rt' <- chkT r ec mt rt
	    return (ICon i t'', rt')
	Nothing -> errorAtId i (ENotSumElem (ppReadable t))
     t -> errorAtId i (ENotSum (ppReadable t))
tchk' r (Ccase e as) mt = tchkCase r e as mt
tchk' r e@(CStar _ n m) mt = do
    let rt = IKind (n+1) (m+1)
    rt' <- chkT r e mt rt
    return (IKind n m, rt')
tchk' r e@(CVar v) mt =
    if isModuleId v then
        case lookupTypeG v r of
        Just t -> do
            t' <- cloneExpr t
	    rt' <- chkT r e mt t'
	    return (IGVar v, rt')
        _ -> internalError ("Unbound module id: " ++ ppReadable v ++ "\n" ++ ppEnv r) 
    else
        case lookupUId v r of
        Just (i, t) -> do
            t' <- cloneExpr t
	    rt' <- chkT r e mt t'
	    return (ILVar i, rt')
        _ -> errorAtId v EUnbound
tchk' r ea@(CApply f as) mt = do
    (f', tf') <- tchkRed r f Nothing
    tracex ("tchk CApply "++ppDebug (f',tf')) $ return ()
--    ats <- mapM (\ (_,a) -> tchk r a Nothing) as
    let emsg = errorAtExpr ea ENoHide
        doAp tf@(IUniv True (x,ta) tb) as vs | goodAs as = do
            tracex ("doAp start "++ppDebug (f,(x,ta,tb),as,mt)  {- ++ppEnv r -}) $ return ()
            noMoreArgs <- case (mt,as) of
                             (Nothing, []) -> return True
                             (Just t, []) -> do at <- arityOf r t
                                                atf <- arityOf r tf
                                                return (atf <= at)
                             _ -> return False
            if (noMoreArgs && True) then return (tf, reverse vs)
             else do
              a <- findValue' emsg r x tb as mt `setTermMsg` ea
              a' <- cloneExpr a
              ta' <- typeOf r a'
              b <- betaEqT r ta ta'
              unless b $
                  typeError2 r ("doAp "++ppDebug (a',ta')) "" ea ta' ta
              tf' <- whnfT ea r (subst x ta a' tb)
              tracex ("doAp end "++ppDebug ((f,a',tf'),(x,ta,tb),as,mt)) $ return ()
              doAp tf' as ((True, a'):vs)
	doAp (IUniv bb (x,ta) tb) ((bb',ca):as) vs = do
            unless (bb==bb') $
                err (getExprPosition ca, EHide bb (ppString ca))
            (a,t) <- tchk r ca (Just ta)
            tracex ("doAp arg "++ppDebug(x,ta,a,tb)) $ return ()
            tf' <- whnfT ea r (subst x ta a tb)
	    doAp tf' as ((bb,a):vs)
	doAp tf (_:_) _ =
	    typeError2 r "extra arg" "Maybe an extra argument?" ea tf iFunc
	doAp tf [] vs = return (tf, reverse vs)
	goodAs [] = True
	goodAs ((False,ca):_) = True
	goodAs _ = False
    tracex2 ("apply " ++ ppDebug tf') $ return ()
    (rt, vs) <- doAp tf' as []
    -- XXX could check for missing arg here
    rt' <- chkT r ea mt rt
    return (foldl (\ f (h,a) -> IApply h f a) f' vs, rt')
tchk' r e@(CLit _ l@(LInteger i)) mt@(Just t) = do
    b <- betaEqT r t intType
    if b then
        return (ILit intType (LInt i), intType)
     else
        tLit r e (litType l) l mt
tchk' r e@(CLit _ l) mt = tLit r e (litType l) l mt
tchk' r (CBinOp e1 o e2) mt = tchk' r (CApply (CVar o) [(False,e1), (False,e2)]) mt
tchk' r (Cdo e bs) mt = tchk' r (expandDo e bs) mt
tchk' r (CWarn msg e) mt = do
    (e', t') <- tchk' r e mt
    return (IWarn msg e', t')
tchk' r e@(CHasType el@(CLit _ _) t) mt = do
    (t', et') <- tchk' r t Nothing
    (e', t'') <- tchk' r el (Just t')
    rt <- chkT r e mt t''
    return (e', rt)
tchk' r ec@(CHasType e t) mt = do
    (e', et') <- tchk' r e Nothing
    (t', _) <- tchk' r t Nothing
    t'' <- whnfT t r t'
    case et' of
     IProduct ss ->
        case t'' of
	IProduct ss' ->
	    case (map fst ss' \\ map fst ss) ++ -- check for missing labels
	         ([ i | (i, (_, _, Just _)) <- ss' ] \\ [ i | (i, (_, _, Just _)) <- ss' ]) -- check for concreteness
	         of --- XXX mt
	    [] -> do
	        e'' <- iOpenSimpl e' ss' (canonIRecord [(i, (t, ILVar ui, me /= Nothing)) | (i, (ui, t, me)) <- ss'])
		rt <- chkT r ec mt t''
		return (e'', rt)
	    _ -> errorAtExpr e (ENoCoerce (ppReadable t))
	_ -> errorAtExpr t (ENotProductCoerceT (ppReadable (iToC False t'')))
     _ -> errorAtExpr e (ENotProductCoerce (ppReadable (iToC False et')))
-- XXX handle casting of any type
tchk' r (Cif c t e) mt = tchk' r (Ccase c [(CPCon trueId [], t), (CPCon falseId [], e)]) mt
tchk' r e@(Cnative s t) mt = do
    (t', _) <- tchk' r t Nothing
    rt <- chkT r e mt t'
    return (iNative s t', rt)

-- XXX can do better than testing mt at the end
tchk' r e@(CEqProof c) mt = do
    (eh, et, t, p) <- tchkEqP r c
    rt <- chkT r e mt (iEquiv t eh et)
    return (p, rt)

tchk' r (CHole e) mt = tchk' r e mt

--tchk' _ e _ =  error ("tchk: "++ppAll e)

{-
eqs ::=	exp
	eqs ={ DEF } exp
	eqs ={ prf } exp 
l(exp) = exp
l(eqs ={ _ } exp) = exp
h(exp) = exp
h(eqs ={ _ } exp) = h(eqs)

	E[e]               =  refl e ::      e === e
	E[eqs ={ DEF } e]  =  E[eqs] :: h(eqs) === e
	E[eqs ={ p } C[e]] =  trans h(eqs) l(eqs) C[e]
				    (E[eqs])
				    (subst (\ (x::T) -> C[x]) p :: C[l(eqs)] === C[e])
				     :: h(eqs) === e
				IF   e::T
-}
tchkEqP :: Env -> CEqChain -> MT (IExpr, IExpr, IExpr, IType) -- first, last, type, proof
tchkEqP r (CEqAtom e) = do
    (e', t) <- tchk r e Nothing
--  XXX check that t has kind #
    return (e', e', t, iRefl t e')
tchkEqP r c0@(CEqDef c e) = do
    (eh, et, tt, ep) <- tchkEqP r c
    (e', _) <- tchk r e (Just tt)
    chkT r (CEqProof c0) (Just et) e'
    return (eh, e', tt, ep)
tchkEqP r c0@(CEqBy c p e) = do
    (eh, et, tt, ep) <- tchkEqP r c
    (p', pt) <- tchkRed r p Nothing
    (e', _) <- tchk r e (Just tt)
    case pt of
     IApply _ (IApply _ (IApply _ (ISelect (IGVar idId) idEq) ty) e1) e2 | idId == idIdMod noPosition &&
									   idEq == idEquiv -> do
        ix <- genSym
	let pos = getExprPosition e
	    x = toId (mkTmpUId ix)
	    (ctx, hole) = findDiff (CVar x) (lastEqE c) e
	    (hole', _) = findDiff (CVar x) hole hole	-- search for extra hole
	    cdt = iToC False ty
	    fun = Clam (False, (CArgT x cdt)) ctx
	    -- XXX avoid tchk of esub
	    esub = CApply (CSelect (CVar (idIdMod noPosition)) (idSubst pos)) [(False, fun), (False, p)]
	if hole' /= hole then
	    err (getExprPosition hole', EBadHole)
	 else do
	    --traces (ppReadable (esub, iEquiv tt et e')) $ return ()
	    (esub', _) <- tchk r esub (Just (iEquiv tt et e'))
	    let etr = iTrans tt eh et e' p' esub'
	    return (eh, e', tt, etr)
     t' -> typeError2 r "tchkEqP" "" p t' iAnyEquiv

iRefl :: IType -> IExpr -> IExpr
iRefl t e = iApply (IApply True (ISelect (IGVar (idIdMod noPosition)) idRefl) t) e

iEquiv :: IType -> IExpr -> IExpr -> IExpr
iEquiv t e1 e2 = iApply (iApply (IApply True (ISelect (IGVar (idIdMod noPosition)) idEquiv) t) e1) e2

iTrans t e1 e2 e3 e4 e5 = iApply (iApply (iApply (iApply (iApply (IApply True (ISelect (IGVar (idIdMod noPosition)) idTrans)
								         t)
							         e1)
						         e2)
					         e3)
				         e4)
			         e5

lastEqE (CEqAtom e) = e
lastEqE (CEqDef _ e) = e
lastEqE (CEqBy _ _ e) = e

----

doInline :: IExpr -> [ISign] -> Maybe IExpr -> IExpr -> MT IExpr
doInline e ss (Just b) _ | isSimple b = iOpenSimpl e ss b
doInline _ _  _        e              = return e

litType :: Literal -> IType
litType (LString _) = stringType
litType (LChar _) = charType
litType (LInteger _) = integerType
litType (LFloat _) = doubleType

expandDo :: CExpr -> [CBind] -> CExpr
expandDo e bs = 
    let p = getExprPosition e 
    in  Copen e (COpenArgs [COpenArg [] (idBind p) Nothing Nothing,
                            COpenArg [] (idBind_ p) Nothing Nothing, 
                            COpenArg [] (idReturn p) Nothing Nothing]) (expandBind bs)

expandBind :: [CBind] -> CExpr
expandBind [CBind_ e] = e
expandBind (CBind_ e : bs) = CBinOp e (idBind_ (getExprPosition e)) (expandBind bs)
expandBind (CBind a e : bs) = CBinOp e (idBind (getArgPosition a)) (Clam (False, a) (expandBind bs))
expandBind (CBLet ds : bs) = Clet ds (expandBind bs)

findValue' :: MT IExpr -> Env -> UId -> IType -> [(Bool,CExpr)] -> Maybe IType -> MT IExpr
findValue' p r x tb as mt = do
    tracex ("findValue' " ++ ppDebug tb) $ return ()
    tb' <- whnfI r tb
    tracex ("findValue' done " ++ ppDebug tb') $ return ()
    findValue (findValue p r x tb as mt) r x tb' as mt

findValue :: MT IExpr -> Env -> UId -> IType -> [(Bool,CExpr)] -> Maybe IType -> MT IExpr
findValue p r x tb [] (Just rt) | x `S.elem` iFreeVars tb =
    matchGet' p r x tb rt
findValue p r x (IUniv True (i,ta) tb) as@((False,_):_) mt = 
    findValue' p r x tb as mt
findValue p r x (IUniv bb (i,ta) tb) ((bb',ca):as) mt | bb==bb' =
    if x `S.elem` iFreeVars ta then do
        tracex ("findValue: arg found " ++ ppDebug (x, ta, ca) ++ ppEnvVars (cFreeVars ca) r) $ return ()
        (a, t) <- tchk r ca Nothing
        matchGet' p r x ta t
    else
        findValue' p r x tb as mt
findValue p r x _ _ _ = p

matchGet' :: MT IExpr -> Env -> UId -> IExpr -> IExpr -> MT IExpr
matchGet' p r x ta t = do
    tracex ("matchGet'" ++ ppDebug (x,ta,t)) $ return ()
    me <- matchGet r x ta t
    case me of
     Just e -> return e
     _ -> p
          --internalError ("matchGet: "++ (ppReadable (x,ta,t)))

matchGet :: Env -> UId -> IExpr -> IExpr -> MT (Maybe IExpr)
matchGet r i x y =
    if i `S.elem` iFreeVars x then
        let try x' y' = do
	     case (x', y') of
              (IUniv _ (u,a1) (IApply b (ILVar i') (ILVar u')),
               IUniv _ (v2,a2) e2) | i==i' && u==u' -> return (Just (Ilam b (v2,a2) e2))
	      (ILVar v, _) -> if v==i then return (Just y) else return Nothing
	      (ISum cs, ISum cs') | all nonDep cs && all nonDep cs' -> foldl pick (return Nothing) (zipWith f cs cs')
		       where f (_, ts) (_, ts') = foldl pick (return Nothing) (zipWith (matchGet r i) (map snd ts) (map snd ts'))
			     nonDep (_, its) = all (isDummyUId . fst) its
	      (IUniv _ (v,a) b, IUniv _ (v',a') b') ->
                 pick (matchGet r i a a') (matchGet (extendEnv v (local a) r) i b (subst v a' (ILVar v') b'))
	      (IApply _ f a, IApply _ f' a') -> pick (matchGet r i f f') (matchGet r i a a')
	      -- IProduct
	      -- XXX should add more here XXX
	      _ -> tracex ("matchGet mismatch " ++ ppAll(x',y')) $ return Nothing
	 in do  tracex ("matchGet enter " ++ ppDebug(i,x,y)) $ return ()
	        pick (try x y)
		     (do x' <- whnfI r x
		         y' <- whnfI r y
		         tracex ("matchGet eval " ++ ppDebug(i,x',y')) $ return ()
		         try x' y')
    else
        return Nothing

pick :: MT (Maybe a) -> MT (Maybe a) -> MT (Maybe a)
pick mx my = do
    x <- mx
    case x of
     Nothing -> my
     _ -> return x

tLit r e t l mt = do
    rt' <- chkT r e mt t
    return (ILit t l, rt')

tchkRed :: Env -> CExpr -> Maybe IType -> MT (IExpr, IType)
tchkRed r e mt = do
    (e', t') <- tchk r e mt
    t'' <- whnfT e r t'
    return (e', t'')

tchkPat :: Env -> CPat -> MT Env
tchkPat r p = undefined -- XXX

tchkCase :: Env -> CExpr -> [(CPat, CExpr)] -> Maybe IType -> MT (IExpr, IType)
tchkCase r e as mt = do
    let pos = getExprPosition e
    (e', dt) <- tchkRed r e Nothing
    (v, bind) <- case e' of 
		 ILVar v -> return (v, id)
		 _ -> do v' <- makeVar Nothing
			 return (v', Ilet [(v', (dt, e'))])
    t <- case mt of
	 Just t -> return t
	 Nothing -> 
	     case as of
	     [] -> errorAtExpr e ENoArms
	     (p, e) : _ -> do
--	         (_, r', _) <- tchkPats r (IUniv False (v,dt) undefined) [p]
	         r' <- return r -- XXX need to extend env!  tchkPat r p
		 (_, te) <- tchk r' e Nothing
		 return te
    tracex ("tchkCase: " ++ ppReadable (e, dt)) $ return ()
    e'' <- tchkPatComp pos r [(v, False, dt)] [cClause [p] e | (p, e) <- as] t
    t' <- cloneExpr t
    return (bind e'', t')

applys f es = foldl iApply f es

iUnivs :: [(UId, Bool, IType)] -> IType -> IType
iUnivs xs t = foldr (\ (d, h, t) r -> IUniv h (d,t) r) t xs

tchkRec :: (Env -> MT (IExpr, IType)) -> Env -> StructType -> [CProp] -> [CDef] -> MT (IExpr, IType)
tchkRec mkBody r as ps ds =
    let ds' = [(cDefId d, d) | d <- ds]
        is = map fst ds'
        g = [ (i, cDefFreeVars d `intersect` is) | (i,d) <- ds' ]
        iss = scc g
        dss = map (map (assoc ds')) iss
	doBinds f _  r [] = f r
	doBinds f rd r (ds:dss) = do
	    (rec, res) <- doBind rd ds
	    let rd' = extendEnvs [(i, mdefn t e)                          | (i, t, e, _,    _  ) <- res] rd
	        r'  = extendEnvs [(i, if con then mdefn t e else local t) | (i, t, e, True, con) <- res] r
	    (e, t) <- doBinds f rd' r' (concatMap (map (\x->[x]) . mkConBinds ps) ds ++ dss)
	    let def = (if rec then iLetrecSimpl else iLetSimpl)
	                [(i, (t, e)) | (i, t, Just e, _, _) <- res]
	    tracex ("tchkRec: " ++ ppDebug (e, t, def e)) $ return ()
	    t' <- cloneExpr (def t)
            e' <- cloneExpr (def e)
	    return (e', t')
	doBind :: Env -> [CDef] -> MT (Bool, [(UId, IType, Maybe IExpr, Bool, Bool)])
	doBind r ds = do
	    if isRec ds then
		case filter (not . hasSign) ds of
		d:_ -> errorAtId (cDefId d) ENoSign
		[] ->
	            let ds' = [(cDefId d, d) | d <- ds ]
		        is = map fst ds'
			g = [ (i, cFreeVars (getSign d) `intersect` is) | (i,d) <- ds' ]
			iss = scc g
			dss = map (map (assoc ds')) iss
		    in  case filter isRecursiveSign dss of
			(d:_) : _ ->
                            errorAtId (cDefId d) EBadRecursion
		        [] -> do
		    	    let f [] r uis = return (r, reverse uis)
			        f ([d] : dss) r uis = do
				    ui <- mkUniqId (cDefId d)
				    (t, _) <- tchk r (getSign d) Nothing
				    f dss (extendEnv ui (local t) r) (ui : uis)

				g [] _ xs = return (reverse xs)
				g ((ui, d):ds) r xs = do
				    ite@(_, t, e, _, _) <- doBind1 r ui d
				    let r' = extendEnv ui (mdefn t e) r
				    g ds r' (ite:xs)
			    (r', uis) <- f dss r []
			    xs <- g (zip uis (concat dss)) r' []
{-
			    xs <- zipWithM (doBind1 r') uis (concat dss) -- XXX extend from local to defn as we go along?
-}
			    return (True, reverse xs)
	     else do
	        uis <- mapM mkUniqId (map cDefId ds)
	        xs <- zipWithM (doBind1 r) uis ds
	        return (False, xs)
	doBind1 r i def@(CDef _ d) = do
	    let (pub, con) = getPubCon ps def
	    (e, t) <- tchkDefn r as d
	    return (i, t, e, pub, con)
    in  case findDup is of
        i:i':_ -> errorAtId i (EDuplicate (remPositionFile (getIdPosition i')))
	_   -> doBinds mkBody r r dss

mkConBinds :: [CProp] -> CDef -> [CDef]
mkConBinds ps d@(CDef _ (Cdata i as cs)) =
    let (pub, con) = getPubCon ps d
        ps' = [if pub && con then Cpublic else Cprivate, if con then Cconcrete else Cabstract]
        asT = zip (repeat True) as
        asF = zip (repeat False) as
	mkArrow (CSType t) r = CArrow False t r
	mkArrow (CSId i t) r = CUniv (False, CArgT i t) r
    in  [CDef ps' (CValueT c asT (foldr mkArrow t ts) (CCon c t)) |
         (c, ts) <- cs, let t = cApply (CVar i) (mapSnd (CVar . argId) asF) ]
mkConBinds _ _ = []

getCVars :: [[CPat]] -> [Maybe FString]
getCVars [] = internalError "getCVars"
getCVars pps = 
    let mvss = map (\ ps ->
	    [ case p of CPVar i   | not (isDummyId (argId i)) -> Just (getIdFString (argId i))
	                CPAs  i _ | not (isDummyId (argId i)) -> Just (getIdFString (argId i))
	                _ -> Nothing
	      | p <- ps ]) pps
	pick mvs = foldr f Nothing mvs
	      where f (Just v) _ = Just v
		    f Nothing mv = mv
    in  map pick (transpose mvss)

cClause ps e = CClause (zip (repeat False) ps) e

fixHaskellType :: Env -> CType -> CType
fixHaskellType r t =
    if haskellFlag then
        let fvs = [ v | v <- cFreeVars t, lookupUId v r == Nothing ]
        in  foldr (\ v -> CUniv (True, CArg False v)) t fvs
    else
        t

tchkDefn :: Env -> StructType -> CDefn -> MT (Maybe IExpr, IType)
tchkDefn r as (CValueT i args t e) = 
    tchkDefn r as (CValueS i (foldr CUniv t args) [CClause (mapSnd CPVar args) e])
tchkDefn r as (CValueS i t pses) = do
    let th = fixHaskellType r t
    (t', k) <- tchk r th Nothing
    unless (isKind k) $
        errorAtId i (EFieldNonKind (ppString t))
    tchkValueX r as i t' pses
tchkDefn r as (CValueP i pses) =
    case as r i of
    Nothing -> errorAtId i ENoSign
    Just t -> tchkValueX r as i t pses
tchkDefn r as d@(Ctype i args t) = 
    tchkDefn r as (CValueS i (foldr cUniv' (CStar (getIdPosition i) 0 0) args) [cClause (map CPVar args) t])
tchkDefn r _ (Cdata i args cs) = do
    (e', t') <- tchk r (foldr clam' (CSum cs) args) Nothing
    chkT r (CVar i) (Just iStar) (dropNUniv (length args) t')
    return (Just e', t')
tchkDefn r as (CValue i e) = do
    (e', t') <- tchk r e (as r i)
    return (Just e', t')
{-
tchkDefn r _ (CNative _ t s) = do
    (t', _) <- tchk r t Nothing
    t'' <- cloneExpr t'
    return (Just (ILit t'' (LNative s)), t')
-}
tchkDefn r as (CNative i t s) = tchkDefn r as (CValueT i [] t (Cnative s t))
tchkDefn r _ (CDSign i t) = do
    (t', k) <- tchk r t Nothing
    unless (isKind k) $
        errorAtId i (EFieldNonKind (ppString t))
    return (Nothing, t')
tchkDefn r _ (Cpostulate i t) = do
    (t', k) <- tchk r t Nothing
    unless (isKind k) $
        errorAtId i (EFieldNonKind (ppString t))
    case k of
        IKind 0 _ -> return (Just undefinedValue, t')
        IKind 1 _ -> return (Just undefinedType, t')
        IKind 2 _ -> return (Just undefinedTypeType, t')
        tt        -> internalError ("tchkDefn: postulate " ++ ppReadable tt)
tchkDefn r _ (CDopen e is) = internalError ("CDopen")

tchkValueX :: Env -> StructType -> Id -> IType -> [CClause] -> MT (Maybe IExpr, IType)
tchkValueX r as i t' pses@(CClause hps _ : _) = do
    unless (allSame [length hps | CClause hps _ <- pses]) $
        errorAtId i EVaryingArgs
    unless (allSame [map fst hps | CClause hps _ <- pses]) $
        errorAtId i EVaryingHiddenArgs
    let pos = getIdPosition i
    (hts, rt, pss) <- takeNArgs pos r (transpose [ map snd ps | CClause ps _ <- pses ]) (map fst hps) t'
    let pses' = if null pss then pses else zipWith cClause (transpose pss) [ e | CClause _ e <- pses]
    tracex ("tchkDefn " ++ ppDebug (i, hts, rt, (pses, pses'))) $ return ()
    let r' = foldr (\ (v,_,t) -> extendEnv v (local t)) r hts
    e <- tchkPatComp pos r' hts pses' rt
    tracex ("tchkDefn'' " ++ ppDebug (i, (foldr (\ (v,h,t) e -> Ilam h (v,t) e) e hts), t')) $ return ()
    return (Just (foldr (\ (v,h,t) e -> Ilam h (v,t) e) e hts), t')

data XClause = XClause [CPat] CExpr [(Id,UId)]

instance PPrint XClause where
    pPrint d p (XClause ps e _) = pPrint d p (CClause (zip (repeat False) ps) e)

getXClausePosition (XClause [] e _) = getExprPosition e
getXClausePosition (XClause (p:_) _ _) = getPatPosition p

tchkPatComp :: Position -> Env -> [(UId, Bool, IType)] -> [CClause] -> IType -> MT IExpr
tchkPatComp pos r hts pses rt = tPatComp pos r hts [XClause (map snd ps) e [] | CClause ps e <- pses]
                                                   (iNoMatch pos rt) rt where 
 tPatComp pos r ds cs def rhst = pc ds cs
  where 
       pc :: [(UId, Bool, IType)] -> [XClause] -> MT IExpr
       pc [] [] = internalError "tPatComp pc"
       pc [] [XClause [] e iis] = do
           let r' = foldr (\ (o,n) r -> addAlias o n r) r iis
           (e', _) <- tchk r' e (Just rhst)
           return e'
       pc [] (c1 : c2 : _)  = err (getXClausePosition c1, EOverlap (getXClausePosition c2))
       pc ((d,h,st):ds) cs = do
	   when (isDummyUId d) $ internalError ("tPatComp _ : " ++ ppReadable cs)
	   st' <- whnfI r st
	   tracex ("enter tPatComp " ++ ppDebug (d, st', cs)) $ return ()
           let (ncon, cNames) =
                   case st' of 
                   ISum cts -> (length cts, map (getIdFString . fst) cts)
                   _ -> (1000000, []) -- non-sum only for Char etc.
	       getP :: XClause -> (CPat, XClause)
	       getP (XClause (p:ps) e iis) = (p, XClause ps e iis)
	       cmpPat :: (CPat, XClause) -> (CPat, XClause) -> Ordering
	       cmpPat (CPCon c _, _) (CPCon c' _, _) = c `compare` c'
	       cmpPat (CPCon c _, _) (CPLit _ _, _) = LT
	       cmpPat (CPLit _ _, _) (CPCon c _, _) = GT
	       cmpPat (CPLit _ l, _) (CPLit _ l', _) = l `compare` l'
	       cmpPat (CPAs _ p, x) p' = cmpPat (p, x) p'
	       cmpPat p (CPAs _ p', x) = cmpPat p (p', x)
	       cmpPat (p1,_) (p2,_) = internalError ("No match in cmpPat: " ++ ppDebug(p1,p2))
	       isIPCon (CPCon _ _) = True
	       isIPCon (CPLit _ _) = True
	       isIPCon (CPVar _) = False
	       isIPCon (CPAs _ p) = isIPCon p
               chkPatArgs n (CPCon c ps, _) | n > n' = errorAtId c EMissingPatArg
                                            | n < n' = errorAtId c EExtraPatArg
                                            | otherwise = return ()
                                       where n' = length ps
	       doGroup :: IExpr -> [(CPat, XClause)] -> MT ICaseArm
	       doGroup ndef g@((CPCon c _, _) : _) = do
        	    --traces ("doGroup " ++ ppReadable g) $ return ()
                    its <- do
		        st'' <- cloneExpr st'
                        case st'' of 
                         ISum cts -> 
                            case lookup c cts of
                            Nothing -> errorAtId c (ENotInType (ppReadable st))
                            Just its -> return its
                         _ -> errorAtId c (ENotSumCase (ppReadable st))
                    mapM_ (chkPatArgs (length its)) g
		    vs <- makeVars (getCVars [ ps | (CPCon _ ps, _) <- g])
		    let mkas _ [] = []
			mkas (v:vs) ((i,t):its) = (v, t) : mkas vs (mapSnd (iNSubstId1 i v) its)
		        as = mkas vs its
		        cval = applys (ICon c st') (map ILVar vs)
		    e <- tPatComp pos (extendEnv d (defn st' cval) r)
			         ([(v, False, t) | (v, t) <- as] ++ ds)
			         [ XClause (ps ++ ps') e iis | (CPCon _ ps, XClause ps' e iis) <- g ] 
				 ndef rhst
		    return (ICCon c, (as, e))
	       doGroup ndef g@((CPLit pos l, _) : _) = do
                    let tl = litType l
                    chkT r (CLit pos l) (Just st') tl
		    e <- tPatComp pos (extendEnv d (defn st' (ILit tl l)) r) ds
			         (map snd g) ndef rhst
		    return (ICLit tl l, ([], e))
	       doGroup _ g = internalError ("doGroup: " ++ ppDebug g)
	       expandAs :: (CPat, XClause) -> (CPat, XClause)
	       expandAs (CPAs i p, XClause ps rhs iis) = -- XXX check type
		    expandAs (p, XClause ps rhs ((argId i, d) : iis))
	       expandAs x = x
	       warn v e = if getIdFString v `elem` cNames then 
	       		      CWarn (getIdPosition v, WBoundConstr (getIdString v)) e
			  else
			      e
	       (pcs', pcs'') = span (isIPCon . fst) (map getP cs)
	       pcss = map (map expandAs) (sortGroup cmpPat pcs')
	       pcss :: [[(CPat, XClause)]]
	       (dpcs, xpcs) = span (not . isIPCon . fst) pcs''
               constrs = [ ICCon c | (CPCon c _, _) : _ <- pcss ] ++ 
                         [ ICLit (litType l) l | (CPLit _ l, _) : _ <- pcss ]
	   unless (null xpcs) $
	       err (pos, EBadPatterns "")
	   def' <- case [ let v = argId a in XClause ps (warn v e) ((v,d):iis) | (CPVar a, XClause ps e iis) <- dpcs ] of -- XXX check arg type
		   [] -> cloneExpr def
		   ps  -> tPatComp pos (extendEnv d (defnot st constrs) r) ds ps def rhst
	   def'' <- if length pcss == ncon then do
	                t <- cloneExpr (iUnivs ds rhst)
		        return (iImpossible t)
		    else
			return def'
	   arms <- mapM (doGroup def') pcss
	   eret <- if null arms then
	               case st' of
		       ISum _ -> return def''
		       _ -> return def'
		   else do
                       --traces ("rhst "++ppDebug rhst) $ return ()
		       caset <- cloneExpr (IUniv h (d,st') rhst)
	               return (Icase (ILVar d) arms def'' caset)
	   tracex ("exit tPatComp " ++ ppDebug (d, st', cs, eret)) $ return ()
	   return eret

takeNArgs :: Position -> Env -> [[CPat]] -> [Bool] -> IType -> MT ([(UId, Bool, IType)], IType, [[CPat]])
takeNArgs pos r [] _ t = return ([], t, [])
takeNArgs pos r (ps:pss) (h:hs) t = do
    t' <- whnfI r t
    case t' of
     IUniv h' a@(ui, tp) tb ->
        if h == h' then do
            let mvs = [ case p of CPVar i   | not (isDummyId (argId i)) -> Just (getIdFString (argId i))
	                          CPAs  i _ | not (isDummyId (argId i)) -> Just (getIdFString (argId i))
	                          _ -> Nothing
		 	| p <- ps ]
		pick mvs = foldr f Nothing mvs
		    where f (Just v) _ = Just v
			  f Nothing mv = mv
	    v <- makeVar (case pick mvs of 
	                  mf@(Just _) -> mf
			  Nothing -> Nothing) -- if isDummyUId ui then Nothing else Just (getUIdFString ui))
	    (ts, rt, pss') <- takeNArgs pos (extendEnv v (local tp) r) pss hs (iNSubstId1 ui v tb)
	    tp' <- cloneExpr tp
	    return ((v, h', tp'):ts, rt, ps:pss')
	else if h' && not h then do
            v <- makeVar (if isDummyUId ui then Nothing else Just (getUIdFString ui))
            (ts, rt, pss') <- takeNArgs pos (extendEnv v (local tp) r) (ps:pss) (h:hs) (iNSubstId1 ui v tb)
	    return ((v, h', tp):ts, rt, map (const (CPVar (CArg False (dummyId pos)))) ps : pss')
	else
	    err (pos, ENotHidden)
     _ -> err (pos, ETooManyArgs "")

isRec :: [CDef] -> Bool	    
isRec [d] = cDefId d `elem` cDefFreeVars d
isRec _ = True

isRecursiveSign :: [CDef] -> Bool	    
isRecursiveSign [d] = cDefId d `elem` cFreeVars (getSign d)
isRecursiveSign _ = True

hasSign :: CDef -> Bool
hasSign (CDef _ (CValue _ _)) = False
hasSign (CDef _ (CValueP _ _)) = False
hasSign _ = True

getSign :: CDef -> CType
getSign (CDef _ (CValueT i as t _)) = foldr CUniv t as
getSign (CDef _ (CValueS i t _)) = t
getSign (CDef _ (Ctype i as _)) = foldr cUniv' (CStar (getIdPosition i) 0 0) as
getSign (CDef _ (Cdata i as _)) = foldr cUniv' (CStar (getIdPosition i) 0 0) as
getSign (CDef _ (CNative i t _)) = t
getSign (CDef _ (CDSign i t)) = t
getSign (CDef _ (Cpostulate i t)) = t
-- XXX getSign not total

isSimple (IGVar _) = True
isSimple (ILit _ _) = True
isSimple _ = False

cUniv' a t = CUniv (False, a) t
clam' a t = Clam (False, a) t

tchkArg :: Env -> CArg -> Maybe IType -> MT (UId, IExpr, IType)
tchkArg r (CArg True i) Nothing = 
    errorAtId i (ENoLamSign)
tchkArg r (CArg False i) Nothing = do
    i' <- mkUniqId i
    return (i', iStar, IKind 1 1)
tchkArg r (CArg _ i) (Just t) = do
    i' <- mkUniqId i
    tt <- typeOf r t
    t' <- cloneExpr t
    return (i', t', tt)
tchkArg r (CArgT i t) mt = do
    i' <- mkUniqId i
    (t', tt) <- tchk r t Nothing
    unless (isKind tt) $
        errorAtId i (EVarNonKind (ppString t'))
    rt <- chkT r (CVar i) mt t'
    return (i', t', tt)

dropNUniv :: Int -> IExpr -> IExpr
dropNUniv 0 t = t
dropNUniv n (IUniv _ _ t) = dropNUniv (n-1) t
dropNUniv _ t = internalError ("dropNUniv: " ++ ppReadable t)

okKindOf (IKind _ _) (IKind _ _) =  True
okKindOf _ _ = False

errorAtId :: Id -> (String -> ErrMsg) -> M a b
errorAtId i e = err (getIdPosition i, e (getIdString i))

errorAtExpr :: CExpr -> (String -> ErrMsg) -> M a b
errorAtExpr i e = err (getExprPosition i, e (ppReadable i))

kindError :: (PPrint e) => Env -> Position -> e -> IType -> IType -> MT a
kindError r pos e t1 t2 = 
    let msg' = if doTrace then ppEnv r else "" in
    err (pos,
	 EKindError msg' (ppReadable e) (ppReadable t1) (ppReadable t2) noPosition)

typeError2 :: Env -> String -> String -> CExpr -> IType -> IType -> MT a
typeError2 r s msg e t1 t2 = do
    t1' <- whnf r t1 `setEMsg` const (return t1)
    t2' <- whnf r t2 `setEMsg` const (return t2)
    let t1'' = tRed t1'
	t2'' = tRed t2'
	s1 = if doFull then ppDebug t1'' else ppReadable t1''
	s2 = if doFull then ppDebug t2'' else ppReadable t2''
    let msg' = if doTrace then msg ++ ppEnv r else msg
    --traces (ppReadable ((t1, t2),(t1',t2'),(t1'',t2''),[t1==t2,t1'==t2',t1''==t2''])) $ return ()
    err (getExprPosition e, 
	 ETypeError s msg' (ppReadable e) s1 s2 noPosition) --(getExprPosition t2))
--

iFunc = iArrow iNone iNone
iNone = ILVar (dummyUId noPosition)
iImpossible t = ILit t LImpossible
iNoMatch p t = ILit t (LNoMatch p)

iAnyEquiv :: IExpr
iAnyEquiv = iApply (iApply (ISelect (IGVar (idIdMod noPosition)) idEquiv) iNone) iNone

-----

arityOf :: Env -> IType -> MT Int
arityOf r t = do
    t' <- whnf r t
    case t' of 
       IUniv _ _ t -> fmap (1+) (arityOf r t)
       _ -> return 0

whnfT :: CExpr -> Env -> IExpr -> MT IExpr
whnfT ce r e = whnf r e `setEMsg` const (errorAtExpr ce (ETypeTermination ""))

whnfI :: Env -> IExpr -> MT IExpr
whnfI r e = whnf r e `setEMsg` const (errorAtExpr (iToC False e) (ETypeTermination ""))

betaEqT :: Env -> IExpr -> IExpr -> MT Bool
betaEqT r e e' =
    betaEq r e e' `setEMsg` const (return False)

chkPCol (p@(CPVar a) : ps) = all (==p) ps
chkPCol (CPCon _ _ : ps) = all isPCon ps
isPCon (CPCon _ _) = True
isPCon _ = False

signType :: [CArg] -> CType
signType as = foldr CUniv (CStar noPosition 0 0) (zip (repeat False) as)

setTermMsg :: MT a -> CExpr -> MT a
setTermMsg x e = x `setEMsg` \ msg -> 
    case msg of
    (_, ETermination _) -> err (getExprPosition e, ETermination (ppString e))
    ec -> err ec

----

----

idBind, idBind_, idReturn :: Position -> Id
idBind pos = mkId pos fsBind
idBind_ pos = mkId pos fsBind_
idReturn pos = mkId pos fsReturn

idRefl = mkId noPosition fsRefl
idIdMod pos = mkId pos fsIdMod 
idEquiv = mkId noPosition fsEquiv
idTrans = mkId noPosition fsTrans
idSubst pos = mkId pos fsSubst

----

{-
dump (IUniv _ _ _) = "IUniv"
dump (Ilam _ _ _) = "Ilam"
dump (IProduct _) = "IProduct"
dump (IRecord _) = "IRecord"
dump (ISelect _ _) = "ISelect"
dump (ISum _) = "ISum"
dump (ICon _ _) = "ICon"
dump (Icase _ _ _ _) = "Icase"
dump (IKind _ _) = "IKind"
dump (ILVar _) = "ILVar"
dump (IGVar _) = "IGVar"
dump (IApply _ _ _) = "IApply"
dump (Ilet _ _) = "Ilet"
dump (Iletrec _ _) = "Iletrec"
dump (ILit _ _) = "ILit"
-}

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = mapM f xs >>= return . concat
