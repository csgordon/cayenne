module CITranslate(iToCM, iToC, ifcIToC) where
import Data.List ((\\), group, sort)
import Libs.ListUtil(mapSnd)
import Error(internalError)
import Id
import Position
import PreStrings
import CSyntax
import ISyntax
import PPrint
import IUtil

import Util(traces)

iToCM :: Bool -> IModule -> CModule
iToCM v (IModule d) = CModule [] (head (iToCDs v [d]))

ifcIToC :: Bool -> IInterface -> CInterface
ifcIToC v (IInterface is s) = CInterface is (iToCS v s)

iToC :: Bool -> IExpr -> CExpr
iToC v (IUniv b a@(i, t) e) =
    if isDummyUId i && not v then 
        CArrow b (iToC v t) (iToC v e) 
    else 
        CUniv (b, iToCAT v a) (iToC v e)
iToC v (Ilam b a e) = 
	Clam (b, iToCAT v a) (iToC v e)
iToC v (IProduct as) = CProduct noPosition (iToCSs v (orderIProduct as))
iToC v (IRecord ds) = CRecord [] noPosition (iToCDs v ds)
iToC v (ISelect e i) = CSelect (iToC v e) i
iToC v (ISum cs) = CSum (map (iToCC v) cs)
iToC v (ICon i t) = CCon i (iToC v t)
iToC v (Icase e as d t) = Ccase (iToC v e) (map (iToCArm v) as ++ g d)
  where g (ILit _ LImpossible) = []
	g e = [(CPVar (CArg False (dummyId noPosition)), iToC v e)]
iToC v (IKind n m) = CStar noPosition n m
iToC v (ILVar i) = CVar (toId i)
iToC v (IGVar i) = CVar i
iToC v@False (IApply h e a) = getArgs e [(h, iToC v a)]
  where getArgs (IApply h e a) as = getArgs e ((h, iToC v a):as)
	getArgs f as = CApply (iToC v f) as
iToC v (IApply h f a) = CApply (iToC v f) [(h, iToC v a)]
iToC v (Iletrec ds (Iletrec ds' e)) = iToC v (Iletrec (ds++ds') e)
iToC v (Iletrec ds (Ilet ds' e)) = iToC v (Iletrec (ds++ds') e)
iToC v (Iletrec ds e) = 
{-
    let vss = (filter ((>1) . length ) . group . sort) 
                   [ v | (_, (_, ISelect (ILVar v) _)) <- ds ]
        f (v:_) 
        (ds', opn) = foldr f (ds, id) vss
    in  traces (ppDebug vss) $
        opn (Clet (iToCDr v ds') (iToC v e))
-}
        Clet (iToCDr v ds) (iToC v e)
iToC v (Ilet ds e) = iToC v (Iletrec ds e)
{-
iToC v (Ilet ds (Ilet ds' e)) = iToC v (Ilet (ds++ds') e)
iToC v (Ilet ds e) = Clet (iToCDr v ds) (iToC v e) -- XXX nonrec
-}
iToC v (ILit t (LNative s)) = Cnative s (iToC v t)
iToC _ (ILit _ l) = CLit noPosition l
iToC v (IWarn _ e) = iToC v e
iToC v (IHasType e t) = CHasType (iToC v e) (iToC v t)

iToCA :: Bool -> (UId, IType) -> CArg
iToCA False (i, IKind 0 0) = CArg False (toId i)
iToCA v (i, t) = CArgT (toId i) (iToC v t)

iToCAT :: Bool -> (UId, IType) -> CArg
iToCAT v (i, t) = CArgT (toId i) (iToC v t)

iToCSs :: Bool -> [ISign] -> [CSign]
iToCSs v cs = 
    map (\ (CDef _ d) -> CSignDef d) 
        (iToCDr v [ (ui, (t, e)) | (_, (ui, t, Just e)) <- cs ]) ++ 
    [ iToCS v a | a@(_, (_, _, Nothing)) <- cs ]

-- XXX check that i == toId ui
iToCS :: Bool -> ISign -> CSign
iToCS v (i, (ui, t, Nothing)) = 
    case getUnivs [] t of
    (as, IKind 0 0) -> CSignType i (map (iToCA v) as)
    _ -> CSign i (iToC v t)
iToCS v (i, (ui, t, Just e)) = 
    case iToCDr v [(ui, (t, e))] of
    [CDef _ d] -> CSignDef d

iToCDs :: Bool -> [IDefRec] -> [CDef]
iToCDs _ [] = []
iToCDs v ((i, (t, ILit _ (LNative s), con)):ds) = 
	CDef ps (CNative i (iToC v t) s) : iToCDs v ds
	where ps = (if con then [Cconcrete] else [Cabstract])
-- XXX this is not really correct
iToCDs v@False ((i, (t, e, con)):ds) =
    case getLamsT [] e t of
    (as, ISum cs, IKind 0 0) | hasAllCs -> 
	CDef ps (Cdata i (map (iToCA v . snd) as) (map (iToCC v) cs)) : iToCDs v (dropCon ds)
    	where dropCon ds = filter ( (`notElem` ics) . fst) ds
	      ics = map fst cs
	      ps = if con then [] else [Cabstract]
	      hasAllCs = null (ics \\ map fst ds)
    (as, e, IKind 0 0) -> 
	CDef ps (Ctype i (map (iToCA v . snd) as) (iToC v e)) : iToCDs v ds
	where ps = (if con then [] else [Cabstract])
    (as, e, t')                -> 
	CDef ps (CValueT i (mapSnd (iToCAT v) as) (iToC v t') (iToC v e)) : iToCDs v ds
	where ps = (if con then [Cconcrete] else [])

iToCDs v ((i, (t, e, con)):ds) = 
	CDef ps (CValueT i [] (iToC v t) (iToC v e)) : iToCDs v ds
	where ps = if con then [Cconcrete] else [Cabstract]


iToCDr :: Bool -> [IDef] -> [CDef]
iToCDr v ds = iToCDs v [(toId i,(t,e,True)) | (i,(t,e)) <- ds]

getLamsT :: [(Bool, IArg)] -> IExpr -> IType -> ([(Bool, IArg)], IExpr, IType)
getLamsT as (Ilam b@False a@(i,_) e) (IUniv _ (i',_) t) | toId i' == toId i || isDummyUId i' =
    getLamsT ((b,a):as) e t
getLamsT as e t = (reverse as, e, t)

getUnivs as (IUniv False a t) = getUnivs (a:as) t
getUnivs as t = (reverse as, t)

iToCC :: Bool -> (Id, [(UId, IType)]) -> (Id, [CSumType])
iToCC v (i, ts) = (i, map iToCS ts)
  where iToCS (i, t) = if isDummyUId i then CSType (iToC v t) else CSId (toId i) (iToC v t)

iToCArm :: Bool -> ICaseArm -> (CPat, CExpr)
iToCArm v (ICCon i, (vs, e)) =
    (CPCon i (map (CPVar . CArg False . toId . fst) vs), iToC v e)
iToCArm v (ICLit _ l, (_, e)) =
    (toCPLit l, iToC v e)
  where toCPLit l = CPLit noPosition l

getLams :: [(UId,IType)] -> IExpr -> ([(UId,IType)], IExpr)
getLams as (Ilam _ a e) = getLams (a:as) e
getLams as e = (reverse as, e)

