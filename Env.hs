module Env(Env, Bind, BindExpr, initEnv,
           lookupType, lookupValue, lookupNonCons, extendEnv, extendEnvs, addAlias,
           lookupTypeG, lookupValueG, extendEnvG, extendEnvGs,
	   lookupUId,
           ppEnv, ppEnvVars,
           local, defn, mdefn, defnot) where
import Libs.ListUtil(mapFst)
import PPrint
import ISyntax
import ISyntaxPP
import Id
import Position
import Error(internalError)

import qualified Data.IntMap as M

data BindExpr = BUnknown | BKnown IExpr | BNot [IConstr]
type Bind = (IType, BindExpr)

instance PPrint BindExpr where
   pPrint d p BUnknown = text "?"
   pPrint d p (BKnown e) = pPrint d p e
   pPrint d p (BNot is) = pparen (p>5) $ text"~" ~. pPrint d 5 is

local :: IType -> Bind
local t = (t, BUnknown)

defn :: IType -> IExpr -> Bind
defn t e = (t, BKnown e)

mdefn :: IType -> Maybe IExpr -> Bind
mdefn t Nothing  = local t
mdefn t (Just e) = defn t e

defnot :: IType -> [IConstr] -> Bind
defnot t cs = (t, BNot cs)

data Env = E (M.IntMap Bind) (M.IntMap (UId, IType)) (M.IntMap Bind)

lookupType :: UId -> Env -> Maybe IType
lookupType i (E u l g) =
    case M.lookup (getUIdNo i) u of
    Just (t, _) -> Just t
    _ -> Nothing

lookupValue :: UId -> Env -> Maybe IExpr
lookupValue i (E u l g) =
    case M.lookup (getUIdNo i) u of
    Just (_, BKnown e) -> Just e
    _ -> Nothing

lookupNonCons :: UId -> Env -> Maybe [IConstr]
lookupNonCons i (E u l g) =
    case M.lookup (getUIdNo i) u of
    Just (_, BNot cs) -> Just cs
    _ -> Nothing

lookupUId :: Id -> Env -> Maybe (UId, IType)
lookupUId i (E _ l _) = M.lookup (getIdNo i) l

lookupTypeG :: Id -> Env -> Maybe IType
lookupTypeG i (E u l g) =
    case M.lookup (getIdNo i) g of
    Just (t, _) -> Just t
    _ -> Nothing

lookupValueG :: Id -> Env -> Maybe IExpr
lookupValueG i (E u l g) =
    case M.lookup (getIdNo i) g of
    Just (_, BKnown e) -> Just e
    _ -> Nothing

addAlias :: Id -> UId -> Env -> Env
addAlias o n (E u l g) = 
    case M.lookup (getUIdNo n) u of
    Just (t, _) -> E u (M.insert (getIdNo o) (n, t) l) g

extendEnv :: UId -> Bind -> Env -> Env
extendEnv x b@(t, _) e@(E u l g) = 
    if isDummyUId x then
        e
    else
	E (M.insert (getUIdNo x) b u) (M.insert (getIdNo (toId x)) (x, t) l) g

extendEnvG :: Id -> Bind -> Env -> Env
extendEnvG x b e@(E u l g) = E u l (M.insert (getIdNo x) b g)

extendEnvs :: [(UId, Bind)] -> Env -> Env
extendEnvs nas (E u l g) = E (addMany (mapFst getUIdNo nas) u) (addMany [(getIdNo (toId x), (x, t)) | (x, (t, _)) <- nas] l) g

extendEnvGs :: [(Id, Bind)] -> Env -> Env
extendEnvGs nas (E u l g) = E u l (addMany (mapFst getIdNo nas) g)

addMany :: [(Int, a)] -> M.IntMap a -> M.IntMap a
addMany xs m = foldr (uncurry M.insert) m xs

initEnv :: Env
initEnv = E M.empty M.empty M.empty

ppEnv r@(E u l g) = "Env: ???\n"

ppEnvVars is r =  "Env(vars): ???\n"

