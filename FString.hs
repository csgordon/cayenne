module FString(FString, hmkFString, getFString, StrTable, emptyStrTable, getFStrNo,
	       tmpFString, isTmpFString) where
-- Fast strings may be represented by a number and the real string.
import qualified Data.Map as M
import Error(internalError)
import PPrint
import Util(mkSet)

data FString = N !Int String		-- unique number, actual string

instance Eq FString where
    N n _ == N n' _  =  n == n'

instance Ord FString where
    N _ s `compare` N _ s' = s `compare` s'

instance Show FString where
--    showsType _ = showString "FString"
    showsPrec p (N n s) = showsPrec p s

instance PPrint FString where
    pPrint _ _ x = text (show x)

getFString (N n s) = s	-- ++":"++show n

getFStrNo (N n _) = n

startNo :: Int
startNo = 100		-- just some start number

data StrTable = T !Int (M.Map String FString) deriving (Show)
emptyStrTable = T startNo M.empty

hmkFString :: StrTable -> String -> (StrTable, FString)
hmkFString tbl@(T k ht) s =
    case M.lookup s ht of
	Just fs -> (tbl, fs)
	Nothing -> let fs = N k s
	           in  (T (k+1) (M.insert s fs ht), fs)
instance Eq StrTable
	where _ == _  =  True
	-- Just for convenience

tmpOffs = 1000000000 :: Int

tmpFString :: Int -> String -> FString
tmpFString n s = N (n+tmpOffs) s

isTmpFString :: FString -> Bool
isTmpFString (N n _) = n >= tmpOffs
