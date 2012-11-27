module CUtil(getExprPosition, cDefId, cDefnId, cSignId, getPatPosition, getClausePosition,
	cGlobModuleVars, cGlobIfcVars, cGlobSignVars, getArgPosition, isProp,
	cFreeVars, cDefFreeVars, cSignFreeVars, getPubCon, argId) where
import Data.List(union, (\\))
import Util(unions)
import Position
import Id
import CSyntax
import MiscId
import Error(internalError)

getExprPosition :: CExpr -> Position
getExprPosition (CUniv (_,a) _) = getArgPosition a
getExprPosition (Clam (_,a) _) = getArgPosition a
getExprPosition (CArrow _ t _) = getExprPosition t
getExprPosition (Clet (CDef _ d:_) _) = getDefnPosition d
getExprPosition (CProduct p _) = p
getExprPosition (CRecord _ p _) = p
getExprPosition (Copen e _ _) = getExprPosition e
getExprPosition (CSelect e _) = getExprPosition e
getExprPosition (CSum ((i,_):_)) = getIdPosition i
getExprPosition (CCon i _) = getIdPosition i
getExprPosition (Ccase e _) = getExprPosition e
getExprPosition (CStar p _ _) = p
getExprPosition (CVar i) = getIdPosition i
getExprPosition (CApply f _) = getExprPosition f
getExprPosition (CLit p _) = p
getExprPosition (CBinOp e _ _) = getExprPosition e
getExprPosition (Cdo e _) = getExprPosition e
getExprPosition (CWarn _ e) = getExprPosition e
getExprPosition (CHasType e _) = getExprPosition e
getExprPosition (Cif c _ _) = getExprPosition c
getExprPosition (Cnative _ e) = getExprPosition e
getExprPosition (CEqProof c) = getEqProofPosition c
getExprPosition (CHole e) = getExprPosition e

getEqProofPosition (CEqAtom e) = getExprPosition e
getEqProofPosition (CEqDef c _) = getEqProofPosition c
getEqProofPosition (CEqBy c _ _) = getEqProofPosition c

getPatPosition :: CPat -> Position
getPatPosition (CPCon c _) = getIdPosition c
getPatPosition (CPVar a) = getArgPosition a

getClausePosition :: CClause -> Position
getClausePosition (CClause ((_,p):_) _) = getPatPosition p
getClausePosition (CClause [] e) = getExprPosition e

getDefnPosition :: CDefn -> Position
getDefnPosition (CValueT i _ _ _) = getIdPosition i
getDefnPosition (CValueS i _ _) = getIdPosition i
getDefnPosition (CValueP i _) = getIdPosition i
getDefnPosition (Ctype i _ _) = getIdPosition i
getDefnPosition (Cdata i _ _) = getIdPosition i
getDefnPosition (CValue i _) = getIdPosition i
getDefnPosition (CNative i _ _) = getIdPosition i
getDefnPosition (CDSign i _) = getIdPosition i
getDefnPosition (Cpostulate i _) = getIdPosition i
getDefnPosition (CDopen e _) = getExprPosition e

getSignPosition :: CSign -> Position
getSignPosition (CSign i _) = getIdPosition i
getSignPosition (CSignDef d) = getDefnPosition d
getSignPosition (CSignType i _) = getIdPosition i

getArgPosition :: CArg -> Position
getArgPosition (CArg _ i) = getIdPosition i
getArgPosition (CArgT i _) = getIdPosition i

cDefId :: CDef -> Id
cDefId (CDef _ d) = cDefnId d

cDefnId :: CDefn -> Id
cDefnId (CValueT i _ _ _) = i
cDefnId (CValueS i _ _) = i
cDefnId (CValueP i _) = i
cDefnId (Ctype i _ _) = i
cDefnId (Cdata i _ _) = i
cDefnId (CValue i _) = i
cDefnId (CNative i _ _) = i
cDefnId (CDSign i _) = i
cDefnId (Cpostulate i _) = i
cDefnId (CDopen _ _) = dummyId noPosition -- XXX internalError ("cDefnId CDopen")

cSignId :: CSign -> Id
cSignId (CSign i _) = i
cSignId (CSignDef d) = cDefnId d
cSignId (CSignType i _) = i

cGlobModuleVars :: CModule -> [Id]
cGlobModuleVars (CModule _ d) = filter isModuleId (cDefFreeVars d)
--cGlobModuleVars _ = []

cGlobIfcVars :: CInterface -> [Id]
cGlobIfcVars (CInterface _ s) = cGlobSignVars s
cGlobIfcVars _ = []

cGlobSignVars :: CSign -> [Id]
cGlobSignVars (CSign _ t) = filter isModuleId (cFreeVars t)
cGlobSignVars (CSignDef d) = filter isModuleId (cDefnFreeVars d)
cGlobSignVars (CSignType _ as) = filter isModuleId (foldr doArg [] as)

cFreeVars :: CExpr -> [Id]
cFreeVars (CUniv (_,a) e) =
    let (i, vs) = cArgFreeVars a
    in  (cFreeVars e `union` vs) \\ [i]
cFreeVars (Clam (_,a) e) =
    let (i, vs) = cArgFreeVars a
    in  (cFreeVars e `union` vs) \\ [i]
cFreeVars (CArrow _ f a) = cFreeVars f `union` cFreeVars a
cFreeVars (Clet ds e) = 
    (unions (map cDefFreeVars ds) `union` cFreeVars e)
    \\ [ cDefId d | d <- ds, (True, _) <- [getPubCon [] d] ]
cFreeVars (CProduct _ ss) = unions (map cSignFreeVars ss) \\ map cSignId ss
cFreeVars (CRecord _ _ ds) = unions (map cDefFreeVars ds) \\ map cDefId ds
cFreeVars (Copen e COpenAll b) = -- XXX wrong
    union (cFreeVars e) (cFreeVars b)
cFreeVars (Copen e (COpenArgs as) b) = 
    let (is, vss) = unzip (map cOpenArgFreeVars as)
        cOpenArgFreeVars (COpenArg _ i (Just t) _) = (i, cFreeVars t)
        cOpenArgFreeVars (COpenArg _ i Nothing _) = (i, [])                                           
    in  union (cFreeVars e) (union (cFreeVars b) (unions vss) \\ is)
cFreeVars (CSelect e _) = cFreeVars e
cFreeVars (CSum cs) = cSummandsFreeVars cs
cFreeVars (CCon _ e) = cFreeVars e
cFreeVars (Ccase e arms) = cFreeVars e `union` (cArmsFreeVars arms)
cFreeVars (CStar _ _ _) = []
cFreeVars (CVar i) = [i]
cFreeVars (CApply f as) = cFreeVars f `union` unions (map (cFreeVars . snd) as)
cFreeVars (CLit _ (LString _)) = [stringModId]
cFreeVars (CLit _ (LChar _)) = [charModId]
cFreeVars (CLit _ (LInteger _)) = [intModId, integerModId]
cFreeVars (CLit _ _) = []
cFreeVars (CBinOp e1 o e2) = union [o] (cFreeVars e1 `union` cFreeVars e2)
cFreeVars (Cdo e bs) = unions (cFreeVars e : map cBindFreeVars bs)
cFreeVars (CHasType e t) = cFreeVars e `union` cFreeVars t
cFreeVars (Cif c t e) = cFreeVars c `union` cFreeVars e `union` cFreeVars t
cFreeVars (CWarn _ e) = cFreeVars e
cFreeVars (Cnative _ e) = cFreeVars e
cFreeVars (CEqProof c) = cFreeVarsEqP c
cFreeVars (CHole e) = cFreeVars e

cFreeVarsEqP (CEqAtom e) = cFreeVars e
cFreeVarsEqP (CEqDef c e) = cFreeVarsEqP c `union` cFreeVars e
cFreeVarsEqP (CEqBy c p e) = cFreeVarsEqP c `union` cFreeVars p `union` cFreeVars e

-- XXX
cBindFreeVars (CBind a e) = 
    let (i, vs) = cArgFreeVars a
    in  (cFreeVars e `union` vs) \\ [i]
cBindFreeVars (CBind_ e) = cFreeVars e
cBindFreeVars (CBLet ds) = unions (map cDefFreeVars ds)

cFreeVarsM Nothing = []
cFreeVarsM (Just e) = cFreeVars e

cSummandsFreeVars cs = unions (map (\ (i, its) -> foldr f [] its) cs)
  where f (CSId i t) vs = cFreeVars t `union` (vs \\ [i])
	f (CSType t) vs = cFreeVars t `union` vs
cArmsFreeVars arms = unions (map (\ (p, e) -> cFreeVars e `union` doPat p []) arms)

cSignFreeVars :: CSign -> [Id]
cSignFreeVars (CSign i t) = cFreeVars t
cSignFreeVars (CSignDef d) = cDefnFreeVars d
cSignFreeVars (CSignType i as) = foldr doArg [] as

cDefFreeVars :: CDef -> [Id]
cDefFreeVars (CDef _ d) = cDefnFreeVars d

cDefnFreeVars :: CDefn -> [Id]
cDefnFreeVars (CValueT _ as t e) = foldr doArg (cFreeVars t `union` cFreeVars e) (map snd as)
cDefnFreeVars (CValueS _ t cs) = cFreeVars t `union` unions (map cClauseFreeVars cs)
cDefnFreeVars (CValueP _ cs) = unions (map cClauseFreeVars cs)
cDefnFreeVars (Ctype _ as t) = foldr doArg (cFreeVars t) as
cDefnFreeVars (Cdata _ as cs) = foldr doArg (cSummandsFreeVars cs) as
cDefnFreeVars (CValue _ e) = cFreeVars e
cDefnFreeVars (CNative _ t _) = cFreeVars t
cDefnFreeVars (CDSign _ t) = cFreeVars t
cDefnFreeVars (Cpostulate _ t) = [errorModId] `union` cFreeVars t
cDefnFreeVars (CDopen e COpenAll) = cFreeVars e	-- XXX WRONG
cDefnFreeVars (CDopen e (COpenArgs as)) = foldr union (cFreeVars e) [ cFreeVars t | COpenArg _ _ (Just t) _ <- as ]

cClauseFreeVars (CClause ps e) = foldr doPat (cFreeVars e) (map snd ps)

doPat (CPVar a) is = doArg a is
doPat (CPCon i as) is = foldr doPat is as
doPat (CPLit _ _) is = is
doPat (CPAs a p) is = doArg a (doPat p is)

doArg :: CArg -> [Id] -> [Id]
doArg (CArg _ i) is = is \\ [i]
doArg (CArgT i t) is = (is \\ [i]) `union` cFreeVars t

cArgFreeVars :: CArg -> (Id, [Id])
cArgFreeVars (CArg _ i) = (i, [])
cArgFreeVars (CArgT i t) = (i, cFreeVars t)

getPubCon :: [CProp] -> CDef -> (Bool, Bool)
getPubCon ps1 (CDef ps2 (Cdata _ _ _)) =
    (isProp Cpublic ps1 ps2 True, isProp Cconcrete ps1 ps2 True)
getPubCon ps1 (CDef ps2 (Ctype _ _ _)) =
    (isProp Cpublic ps1 ps2 True, isProp Cconcrete ps1 ps2 True)
getPubCon ps1 (CDef ps2 _) =
    (isProp Cpublic ps1 ps2 True, isProp Cconcrete ps1 ps2 False)

isProp p ps2 ps1 def =
    p `elem` ps1 ||
    (invProp p `notElem` ps1 && (p `elem` ps2 || 
                                 (invProp p `notElem` ps2 && def)))

invProp Cpublic = Cprivate
invProp Cconcrete = Cabstract

argId :: CArg -> Id
argId (CArg _ i) = i
argId (CArgT i _) = i

