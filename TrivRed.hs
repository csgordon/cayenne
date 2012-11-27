module TrivRed(tRed) where
import qualified UIdSet as S
import ListUtil(mapSnd)
import Id
import ISyntax
import IUtil(spine, iLetSimpl, iLetrecSimpl, iLetrecSCC, patToLam', iUniv, iLam, iFreeVars, canonIProduct)
import Util(assoc, sortFst)
{-import List(sortBy)-}

--import Util(tracex2)
--import PPrint
--import ISyntaxPP

-- Perform trivial reductions.
tRed :: IExpr -> IExpr
tRed (IUniv b (i, t) e) = iUniv b (i, tRed t) (tRed e)
tRed (Ilam  b (i, t) e) = iLam	b (i, tRed t) (tRed e)
tRed (IProduct ss) = canonIProduct [(i, (ui, tRed t, fmap tRed oe)) | (i, (ui, t, oe)) <- ss]
tRed (IRecord ds) = IRecord [(i, (tRed t, tRed e, con)) | (i, (t, e, con)) <- ds] -- canonical order
tRed (ISelect e i) =
    case tRed e of
    IRecord ds -> case assoc ds i of (_, e, _) -> e
    Ilet    ds b -> tRed (Ilet	  ds (ISelect b i))
    Iletrec ds b -> tRed (Iletrec ds (ISelect b i))
    e' -> ISelect e' i
tRed (ISum cs) = ISum [(c, mapSnd tRed ts) | (c, ts) <- cs]
tRed (ICon c t) = ICon c (tRed t)
tRed (Icase e arms d t) =
    let e' = tRed e in
    case spine [] e' of
    (ICon c _, es) -> 
	case lookup (ICCon c) arms of
	Nothing -> tRed d
	Just rhs -> tRed (foldl iApply (patToLam' rhs) es)
    (ILit p l, es) | notNative l -> 
	case lookup (ICLit p l) arms of
	Nothing -> tRed d
	Just rhs -> tRed (foldl iApply (patToLam' rhs) es)
    _ -> 
	let g (c, (as, e)) =
		let e'' = tRed e
		    vs = iFreeVars e''
		    f (i, t) = (if i `S.elem` vs then i else dummyUId (getUIdPosition i), tRed t)
		in  (c, (map f as, e''))
	in  Icase e' (map g arms) (tRed d) (tRed t)
tRed e@(IKind _ _) = e
tRed e@(ILVar _) = e
tRed e@(IGVar _) = e
tRed (IApply h f a) =
    case tRed f of
    Ilam _ (i, t) b -> tRed (iLetSimpl [(i, (t, a))] b)
    f' -> IApply h f' (tRed a)
tRed (Ilet ds e) = 
    case iLetSimpl [(i, (tRed t, tRed e)) | (i, (t, e)) <- ds] (tRed e) of
    e@(Ilet ds' _) | ds' == ds -> e
    e -> tRed e
tRed (Iletrec ds e) =
    case iLetrecSCC [(i, (tRed t, tRed e)) | (i, (t, e)) <- ds] (tRed e) of
    e@(Iletrec ds' _) | sortFst ds' == sortFst ds -> e
    e -> tRed e
tRed e@(ILit t _) = e
tRed (IWarn w e) = IWarn w (tRed e)
tRed (IHasType e t) = IHasType (tRed e) (tRed t)

notNative (LNative _) = False
notNative _ = True
