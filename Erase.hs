module Erase(eraseTypes) where
import Data.List(findIndex)
import Libs.ListUtil(mapSnd)
import PPrint
import Position(noPosition)
import Error(internalError)
import ISyntax
import ISyntaxPP
import Id
import MiscId
import Env
import IReduce(typeOfX {-, levelOf-})
import Util(boolCompress)

import Util(tracex)

eraseTypes :: Bool -> Env -> IModule -> IModule
eraseTypes d r (IModule (i, (t, e, b))) = IModule (i, (impossible, erase d r e, b))

dropE r e = dropT r (typeOfX r e)

dropT r t = 
    tracex ("dropT: " ++ ppDebug t) $
    case typeOfX r t of
    IKind _ n -> n > 0 -- XXX
    _ -> False

impossible = ILit (ILVar (dummyUId noPosition)) LImpossible

erase :: Bool -> Env -> IExpr -> IExpr
erase d r (IUniv _ _ _) = impossible
erase d r (Ilam b (x,t) e) =
    let r' = extendEnv x (local t) r
    in  if d && dropT r t then
            erase d r' e
	else
	    Ilam b (x, impossible) (erase d r' e)
erase d r (IProduct _) = impossible
erase d r (IRecord ds) =
    IRecord [ (i, (impossible, erase d r e, b)) | (i, (t, e, b)) <- ds, not (dropT r t)]
erase d r es@(ISelect e i) =
    case typeOfX r e of
    IProduct ds -> 
        let r' = extendEnvs [(ui, mdefn t me) | (_, (ui, t, me)) <- ds] r
	    ds' = [ (i, (ui, impossible, Nothing)) | (i, (ui, t, _)) <- ds, not (dropT r' t)]
	in  case findIndex ((==i) . fst) ds' of
            Nothing -> internalError ("erase: select k " ++ ppAll es ++ " " ++ ppAll (map fst ds))
            Just k -> IHasType (ISelect (erase d r e) i) (IProduct ds')
    t -> internalError ("erase: select " ++ ppAll es ++ ":: " ++ ppAll t)
erase d r (ISum t) = impossible
erase d r (ICon i e) = ICon i (eraseData r e)
erase d r (Icase e as dd tt) = 
    case tt of
    IUniv _ (_, tt@(~(ISum cts))) _ -> Icase (IHasType (erase d r e) tt') (map f as) (erase d r dd) impossible
	where tt' = eraseData r tt
              f (ICCon c,(as,e)) = (ICCon c, (dropDataArgs r c cts as, (erase d (extendEnvs (g c as ++ mapSnd local as) r) e)))
	      f (ICLit tl l,(_,e)) = (ICLit tl l, ([], (erase d (h r tl l) e)))
--	      f _ = internalError "erase: unimpl literal"
	      g c as = case e of ILVar v -> [(v, defn tt (foldl iApply (ICon c tt) (map (ILVar . fst) as)))]
	                         _ -> []
	      h r tl l = case e of ILVar v -> extendEnv v (defn tt (ILit tl l)) r
	                           _ -> r
    t' -> internalError ("toLML case: " ++ ppAll (e, t') ++ ppEnv r)
erase d r e@(IKind _ _) = impossible
erase d r e@(ILVar v) = e
erase d r e@(IGVar v) = e
erase d r (IApply b f a) = 
    if d && dropE r a then
        erase d r f
    else
        IApply b (erase d r f) (erase d r a)
erase d r (Ilet ds b) =
    let r' = extendEnvs [(i, defn t e) | (i, (t, e)) <- ds] r
    in  xIlet [(i,(impossible,erase d r' e)) | (i,(t,e)) <- ds, not (d && dropT r' t)] (erase d r' b)
erase d r (Iletrec ds b) = 
    let r' = extendEnvs [(i, defn t e) | (i, (t, e)) <- ds] r
    in  xIletrec [(i,(impossible,erase d r' e)) | (i,(t,e)) <- ds, not (d && dropT r' t)] (erase d r' b)
erase d _ e@(ILit _ l) = e
erase d r (IWarn _ e) = erase d r e
erase d r e = internalError ("erase: " ++ ppAll e)

eraseData r (ISum cts) = ISum (map (\ (c, its) -> (c, f r its)) cts)
  where f r [] = []
	f r ((i, t) : its) = 
	    let its' = f (extendEnv i (local t) r) its
	    in  if dropT r t then its' else (i, t) : its'
eraseData r e = e -- some types like System$CharType.Char are abstract
--eraseData r e = internalError ("eraseData: " ++ ppAll e)

dropDataArgs r c cts xs = 
    case lookup c cts of
    Nothing -> internalError ("dropDataArgs: " ++ ppDebug(c, cts))
    Just its -> boolCompress (f r its) xs
		where f r [] = []
		      f r ((i, t) : its) = not (dropT r t) : f (extendEnv i (local t) r) its

xIlet [] e = e
xIlet ds e = Ilet ds e

xIletrec [] e = e
xIletrec ds e = Iletrec ds e
