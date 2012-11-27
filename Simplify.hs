module Simplify(simplify) where
--import ListUtil(mapSnd)
--import qualified UIdSet as S
import Id
import ISyntax
import IUtil
import Env
import TrivRed

simplify :: Env -> IModule -> IModule
simplify r (IModule (i, (t, e, b))) = IModule (i, (tRed t, tRed e, b))

{-
remBind :: IExpr -> IExpr
remBind (IUniv b a e) = iUniv b a e
remBind (Ilam  b a e) = iLam  b a e
remBind (IProduct its) = IProduct (map remBindSign its) -- XXX drop bindings here?
remBind (IRecord ds) = IRecord (map remBindDefRec ds)
remBind (ISelect e i) = ISelect (remBind e) i
remBind (ISum cs) = ISum (mapSnd (map remBind) cs)
remBind (ICon c t) = ICon c (remBind t)
remBind (Icase e cs d t) = Icase (remBind e) (map remBindArm cs) (remBind d) (remBind t)
remBind e@(IKind _ _) = e
remBind e@(ILVar _) = e
remBind e@(IGVar v) = e
remBind (IApply h f a) = IApply h (remBind f) (remBind a)
remBind (Ilet ds b) = iLetSimpl (map remBindDef ds) (remBind b)
remBind (Iletrec ds b) = iLetrecSimpl (map remBindDef ds) (remBind b)
remBind e@(ILit _ _) = e
remBind (IWarn w e) = IWarn w (remBind e)

remBindDefRec :: IDefRec -> IDefRec
remBindDefRec (i, (t, e, b)) = (i, (remBind t, remBind e, b))

remBindDef :: IDef -> IDef
remBindDef (i, (t, e)) = (i, (remBind t, remBind e))

remBindSign :: ISign -> ISign
remBindSign (i, (ui, t, me)) = (i, (ui, remBind t, map remBind me))

remBindArm :: ICaseArm -> ICaseArm
remBindArm (c, (as, e)) = 
    let e' = remBind e
	vs = iFreeVars e'
	f (i, t) = (if i `S.elem` vs then i else dummyUId (getUIdPosition i), remBind t)
    in  (c, (map f as, e'))
-}
