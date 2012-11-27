module TMonad where
import Util(tracex)
import Error
import Position
import FString
import Id
import ISyntax
import IUtil(iNSubstId1)

newtype M s a = M (s -> Either EMsg (s, a))

instance Monad (M s) where
    return a = M $ \ s -> Right (s, a)
    M a >>= f = M $ \ s ->
        case a s of
	Left e -> Left e
	Right (s', b) ->
	    let M f' = f b
	    in  f' s'

instance Functor (M s) where
   fmap f m = m >>= return.f

run :: s -> M s a -> Either EMsg a
run s (M m) = 
    case m s of
    Left e -> Left e
    Right (_, a) -> Right a

err :: EMsg -> M a b
err e = M (\s->Left e)

handle :: M s a -> M s a -> M s a
handle (M a) (M b) = M $ \ s ->
    case a s of
    Left _ -> b s
    r -> r

data TState = TState Int Int
type MT a = M TState a

genSym :: MT Int
genSym = M $ \ (TState i c) -> Right (TState (i+1) c, i)

step :: MT ()
step = M $ \ (TState i c) ->
    if c <= 0 then
        tracex "Looping" $
        Left (noPosition, ETermination "")
    else
        Right (TState i (c-1), ())

-- reduce the count to n %
reduceCount :: Int -> MT a -> MT a
reduceCount n (M m) = M $ \ (TState i c) ->
    let k = min c (max 50 (c*n `div` 100))
    in  case m (TState i k) of
        Left e -> Left e
	Right (TState i' c', a) -> Right (TState i' (c' + c - k), a)

setEMsg :: MT a -> (EMsg -> MT a) -> MT a
setEMsg (M a) f = M $ \ s ->
    case a s of
    Left e -> let M f' = f e in f' s
    r -> r

cloneId :: UId -> MT UId
cloneId i | isDummyUId i = return i
cloneId i = do
    u <- genSym
    return (toUId (toId i) u)

mkUniv :: Bool -> UId -> IType -> IType -> MT IType
mkUniv bb i a b | isDummyUId i = return (IUniv bb (i, a) b)
mkUniv bb i a b = do
    i' <- cloneId i
    return (IUniv bb (i', a) (iNSubstId1 i i' b))

newVar :: FString -> MT UId
newVar fs = do
    u <- genSym
    return (mkUId noPosition fs u)

makeVars :: [Maybe FString] -> MT [UId]
makeVars is = mapM makeVar is

makeVar :: Maybe FString -> MT UId
makeVar Nothing = do u <- genSym
                     return (mkTmpUId u)
makeVar (Just fs) = do u <- genSym
		       return (mkUId noPosition fs u)

mkUniqId :: Id -> MT UId
mkUniqId i | isDummyId i = return (dummyUId (getIdPosition i))
mkUniqId i = do
    u <- genSym
    return (toUId i u)

------
--XXXX
getCount :: MT Int
getCount = M $ \ s@(TState _ c) -> Right (s, c)

