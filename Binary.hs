-- TODO:
-- treat dummyId specially?
-- encode small ints better
-- encode Integer properly
module Binary(iInterfaceToBytes, iInterfaceFromBytes) where
import Data.List(union)
import Data.Maybe(maybeToList)
import Data.Char(chr, ord)
import Util(unions)
import FString
import Id
import Position
import ISyntax
import IUtil(iBoundVars, patToLam)
import Error(internalError)
import qualified Data.IntMap as M

import ISyntaxPP
import PPrint
--import Util(traces)

infixl $$

version = "990609"

type Byte = Char
type Bytes = [Byte]
type ToBin a = a -> Bytes -> Bytes

data Table = Table (M.IntMap UId) (M.IntMap Id)
type BinRes a = (a, Table, Bytes)
type FromBin a = Table -> Bytes -> BinRes a

iInterfaceToBytes :: IInterface -> Bytes
iInterfaceToBytes ii@(IInterface is s) =
    let e = IProduct [s]
	ls = iBoundVars e
	gs = iIds e `union` is
    in  version ++ (listToBin mkUIdBin ls $ listToBin mkIdBin gs $ iInterfaceToBin ii [])

iInterfaceFromBytes :: StrTable -> Bytes -> (StrTable, IInterface)
iInterfaceFromBytes strs bs = 
    case fun undefined (checkVersion bs) (,) $$ listFromBin unUIdBin $$ listFromBin unIdBin of
    ((ls, gs), _, bs') ->
        let (strs', lm)  = foldr (\ (s, u) (t, m) -> case hmkFString t s of (t', fs) -> (t', M.insert u (mkUId noPosition fs u) m)) 
			         (strs, M.empty) ls
	    (strs'', gm) = foldr (\ (s, u) (t, m) -> case hmkFString t s of (t', fs) -> (t', M.insert u (mkId noPosition fs) m)) 
			         (strs', M.empty) gs
	in  case fun (Table lm gm) bs' id $$ iInterfaceFromBin of
	    (ii, _, []) -> (strs'', ii)
	    _ -> internalError "iInterfaceFromBin extra bytes"

iModuleToBin :: ToBin IModule
iModuleToBin (IModule d) bs = iDefRecToBin d bs

iModuleFromBin :: FromBin IModule
iModuleFromBin tbl bs = fun tbl bs IModule $$ iDefRecFromBin

iInterfaceToBin :: ToBin IInterface
iInterfaceToBin (IInterface is s) bs = listToBin idToBin is $ iSignToBin s bs

iInterfaceFromBin :: FromBin IInterface
iInterfaceFromBin tbl bs = fun tbl bs IInterface $$ listFromBin idFromBin $$ iSignFromBin

iExprToBin :: ToBin IExpr
iExprToBin (IUniv False a e) bs = '\x01' : iArgToBin a (iExprToBin e bs)
iExprToBin (IUniv True  a e) bs = '\x02' : iArgToBin a (iExprToBin e bs)
iExprToBin (Ilam  False a e) bs = '\x03' : iArgToBin a (iExprToBin e bs)
iExprToBin (Ilam  True  a e) bs = '\x04' : iArgToBin a (iExprToBin e bs)
iExprToBin (IProduct ss) bs     = '\x05' : listToBin iSignToBin ss bs
iExprToBin (IRecord ds) bs      = '\x06' : listToBin iDefRecToBin ds bs
iExprToBin (ISelect e i) bs     = '\x07' : iExprToBin e (idToBin i bs)
iExprToBin (ICon i t) bs        = '\x08' : idToBin i (iExprToBin t bs)
iExprToBin (Icase e as d t) bs  = '\x09' : (iExprToBin e $ listToBin iArmToBin as $ iExprToBin d $ iExprToBin t bs)
iExprToBin (IKind k l) bs       = '\x0a' : ushortToBin k (ushortToBin l bs)
iExprToBin (ILVar i) bs         = '\x0b' : uIdToBin i bs
iExprToBin (IGVar i) bs         = '\x0c' : idToBin i bs
iExprToBin (IApply False f a) bs= '\x0d' : iExprToBin f (iExprToBin a bs)
iExprToBin (IApply True  f a) bs= '\x0e' : iExprToBin f (iExprToBin a bs)
iExprToBin (Ilet ds e) bs       = '\x0f' : listToBin iDefToBin ds (iExprToBin e bs)
iExprToBin (Iletrec ds e) bs    = '\x10' : listToBin iDefToBin ds (iExprToBin e bs)
iExprToBin (ILit t l) bs        = '\x11' : iExprToBin t (iLitToBin l bs)
iExprToBin (IWarn _ e) bs       = iExprToBin e bs
iExprToBin (ISum cs) bs         = '\x12' : listToBin iSummandToBin cs bs
iExprToBin (IHasType e t) bs    = '\x13' : iExprToBin e (iExprToBin t bs)
--iExprToBin e _ = internalError ("iExprToBin " ++ ppDebug e)

iExprFromBin :: FromBin IExpr
iExprFromBin tbl ('\x01':bs) = fun tbl bs (IUniv False) $$ iArgFromBin $$ iExprFromBin
iExprFromBin tbl ('\x02':bs) = fun tbl bs (IUniv True ) $$ iArgFromBin $$ iExprFromBin
iExprFromBin tbl ('\x03':bs) = fun tbl bs (Ilam  False) $$ iArgFromBin $$ iExprFromBin
iExprFromBin tbl ('\x04':bs) = fun tbl bs (Ilam  True ) $$ iArgFromBin $$ iExprFromBin
iExprFromBin tbl ('\x05':bs) = fun tbl bs IProduct $$ listFromBin iSignFromBin
iExprFromBin tbl ('\x06':bs) = fun tbl bs IRecord $$ listFromBin iDefRecFromBin
iExprFromBin tbl ('\x07':bs) = fun tbl bs ISelect $$ iExprFromBin $$ idFromBin
iExprFromBin tbl ('\x08':bs) = fun tbl bs ICon $$ idFromBin $$ iExprFromBin
iExprFromBin tbl ('\x09':bs) = fun tbl bs Icase $$ iExprFromBin $$ listFromBin iArmFromBin $$ iExprFromBin $$ iExprFromBin
iExprFromBin tbl ('\x0a':bs) = fun tbl bs IKind $$ ushortFromBin $$ ushortFromBin
iExprFromBin tbl ('\x0b':bs) = fun tbl bs ILVar $$ uIdFromBin
iExprFromBin tbl ('\x0c':bs) = fun tbl bs IGVar $$ idFromBin
iExprFromBin tbl ('\x0d':bs) = fun tbl bs (IApply False) $$ iExprFromBin $$ iExprFromBin
iExprFromBin tbl ('\x0e':bs) = fun tbl bs (IApply True ) $$ iExprFromBin $$ iExprFromBin
iExprFromBin tbl ('\x0f':bs) = fun tbl bs Ilet $$ listFromBin iDefFromBin $$ iExprFromBin
iExprFromBin tbl ('\x10':bs) = fun tbl bs Iletrec $$ listFromBin iDefFromBin $$ iExprFromBin
iExprFromBin tbl ('\x11':bs) = fun tbl bs ILit $$ iExprFromBin $$ iLitFromBin
iExprFromBin tbl ('\x12':bs) = fun tbl bs ISum $$ listFromBin iSummandFromBin
iExprFromBin tbl ('\x13':bs) = fun tbl bs IHasType $$ iExprFromBin $$ iExprFromBin

iArgToBin :: ToBin IArg
iArgToBin (i, t) bs = uIdToBin i $ iExprToBin t bs

iArgFromBin :: FromBin IArg
iArgFromBin tbl bs = fun tbl bs (,) $$ uIdFromBin $$ iExprFromBin

iSignToBin :: ToBin ISign
iSignToBin (i, (ui, t, Nothing)) bs = '\x00' : uIdToBin ui (iExprToBin t bs)
iSignToBin (i, (ui, t, Just e))  bs = '\x01' : uIdToBin ui (iExprToBin t (iExprToBin e bs))

iSignFromBin :: FromBin ISign
iSignFromBin tbl ('\x00':bs) = fun tbl bs (\ ui t -> (toId ui, (ui, t, Nothing))) $$ uIdFromBin $$ iExprFromBin
iSignFromBin tbl ('\x01':bs) = fun tbl bs (\ ui t e -> (toId ui, (ui, t, Just e))) $$ uIdFromBin $$ iExprFromBin $$ iExprFromBin

iDefRecToBin :: ToBin IDefRec
iDefRecToBin (i, (t, e, False)) bs = '\x00' : (idToBin i $ iExprToBin t $ iExprToBin e bs)
iDefRecToBin (i, (t, e, True)) bs  = '\x01' : (idToBin i $ iExprToBin t $ iExprToBin e bs)

iDefRecFromBin :: FromBin IDefRec
iDefRecFromBin tbl ('\x00':bs) = fun tbl bs (\ i t e -> (i, (t, e, False))) $$ idFromBin $$ iExprFromBin $$ iExprFromBin
iDefRecFromBin tbl ('\x01':bs) = fun tbl bs (\ i t e -> (i, (t, e, True ))) $$ idFromBin $$ iExprFromBin $$ iExprFromBin

iArmToBin :: ToBin ICaseArm
iArmToBin (ICCon i, (as, e)) bs  = '\x00' : (idToBin i $ listToBin iArgToBin as $ iExprToBin e bs)
iArmToBin (ICLit t l, (_, e)) bs = '\x01' : (iExprToBin t $ iLitToBin l $ iExprToBin e bs)

iArmFromBin :: FromBin ICaseArm
iArmFromBin tbl ('\x00':bs) = fun tbl bs (\ i as e -> (ICCon i, (as, e))) $$ idFromBin $$ listFromBin iArgFromBin $$ iExprFromBin
iArmFromBin tbl ('\x01':bs) = fun tbl bs (\ t l e -> (ICLit t l, ([], e))) $$ iExprFromBin $$ iLitFromBin $$ iExprFromBin

iLitToBin :: ToBin Literal
iLitToBin (LString s) bs = '\x00' : stringToBin s bs
iLitToBin (LChar c) bs   = '\x01' : intToBin (ord c) bs
iLitToBin (LInt i) bs    = '\x02' : integerToBin i bs
iLitToBin (LNative s) bs = '\x03' : stringToBin s bs
iLitToBin LImpossible bs = '\x04' : bs
iLitToBin (LNoMatch (Position s l c)) bs = '\x05' : (stringToBin s $ intToBin l $ intToBin c bs)
iLitToBin (LInteger i) bs = '\x06' : integerToBin i bs

iLitFromBin :: FromBin Literal
iLitFromBin tbl ('\x00':bs) = fun tbl bs LString $$ stringFromBin
iLitFromBin tbl ('\x01':bs) = fun tbl bs (LChar . chr) $$ intFromBin
iLitFromBin tbl ('\x02':bs) = fun tbl bs LInt $$ integerFromBin
iLitFromBin tbl ('\x03':bs) = fun tbl bs LNative $$ stringFromBin
iLitFromBin tbl ('\x04':bs) = fun tbl bs LImpossible
iLitFromBin tbl ('\x05':bs) = fun tbl bs (\ s l c -> LNoMatch (Position s l c)) $$ stringFromBin $$ intFromBin $$ intFromBin
iLitFromBin tbl ('\x06':bs) = fun tbl bs LInteger $$ integerFromBin

iDefToBin :: ToBin IDef
iDefToBin (i, (t, e)) bs = uIdToBin i $ iExprToBin t $ iExprToBin e bs

iDefFromBin :: FromBin IDef
iDefFromBin tbl bs = fun tbl bs (\ x y z -> (x, (y, z))) $$ uIdFromBin $$ iExprFromBin $$ iExprFromBin

iSummandToBin :: ToBin (Id, [(UId,IType)])
iSummandToBin (i, ts) bs = idToBin i $ listToBin iCSToBin ts bs
  where iCSToBin (i, t) bs = uIdToBin i $ iExprToBin t bs

iSummandFromBin :: FromBin (Id, [(UId, IType)])
iSummandFromBin tbl bs = fun tbl bs (,) $$ idFromBin $$ listFromBin iCSFromBin
  where iCSFromBin tbl bs = fun tbl bs (,) $$ uIdFromBin $$ iExprFromBin

listToBin :: ToBin a -> ToBin [a]
listToBin f xs bs = ushortToBin (length xs) $ list xs
  where list [] = bs
	list (x : xs) = f x $ list xs

listFromBin :: FromBin a -> FromBin [a]
listFromBin f tbl bs =
    case ushortFromBin tbl bs of
    (n, _, bs') -> list n [] bs'
  where list 0 xs bs = (reverse xs, tbl, bs)
	list n xs bs = case f tbl bs of (x, _, bs') -> list (n-1::Int) (x:xs) bs'

uIdToBin :: ToBin UId
uIdToBin i bs = intToBin (getUIdNo i) bs

uIdFromBin :: FromBin UId
uIdFromBin tbl bs = fun tbl bs (getUId tbl) $$ intFromBin

idToBin :: ToBin Id
idToBin i bs = intToBin (getIdNo i) bs

idFromBin :: FromBin Id
idFromBin tbl bs = fun tbl bs (getId tbl) $$ intFromBin

ushortToBin :: ToBin Int
ushortToBin i bs = chr (i `mod` 0x100) : chr (i `div` 0x100) : bs

ushortFromBin :: FromBin Int
ushortFromBin tbl (b1:b2:bs) = (ord b1 + ord b2 * 0x100, tbl, bs)

intToBin :: ToBin Int
intToBin i bs = chr (i `mod` 0x100) : chr (i' `mod` 0x100) : chr (i'' `mod` 0x100) : chr (i''' `mod` 256) : bs
  where i' = i `div` 0x100
	i'' = i' `div` 0x100
	i''' = i'' `div` 0x100

intFromBin :: FromBin Int
intFromBin tbl (b1:b2:b3:b4:bs) = (ord b1 + ord b2 * 0x100 + ord b3 * 0x10000 + ord b4 * 0x1000000, tbl, bs)

integerToBin :: ToBin Integer
integerToBin i bs = intToBin (fromInteger i) bs -- XXX

integerFromBin :: FromBin Integer
integerFromBin tbl bs = fun tbl bs toInteger $$ intFromBin

stringToBin :: ToBin String
stringToBin s bs = 
    if any ((> 255) . ord) s then
       let list [] = bs
	   list (c : cs) = ushortToBin (ord c) $ list cs
       in  '\x01' : ushortToBin (length s) (list s)
    else
       '\x00' : ushortToBin (length s) (s ++ bs)

stringFromBin :: FromBin String
stringFromBin tbl ('\x00':bs) =
    case ushortFromBin tbl bs of
    (n, _, bs') -> case splitAt n bs' of (s, bs'') -> (s, tbl, bs'')
stringFromBin tbl ('\x01':bs) =
    case ushortFromBin tbl bs of
    (n, _, bs') -> list n [] bs'
  where list 0 cs bs = (reverse cs, tbl, bs)
	list n cs (b1:b2:bs) = list (n-1::Int) (chr (ord b1 + ord b2 * 0x100) : cs) bs

mkUIdBin :: ToBin UId
mkUIdBin i bs = stringToBin (getUIdString i) (intToBin (getUIdNo i) bs)

mkIdBin :: ToBin Id
mkIdBin i bs = stringToBin (getIdString i) (intToBin (getIdNo i) bs)

unUIdBin :: FromBin (String, Int)
unUIdBin tbl bs = fun tbl bs (,) $$ stringFromBin $$ intFromBin

unIdBin :: FromBin (String, Int)
unIdBin tbl bs = fun tbl bs (,) $$ stringFromBin $$ intFromBin

($$) :: BinRes (a->b) -> FromBin a -> BinRes b
(f, tbl, bs) $$ r =
        case r tbl bs of
            (x, tbl', bs') -> (f x, tbl', bs')

fun :: Table -> Bytes -> a -> BinRes a
fun tbl bs f = (f, tbl, bs)

getUId :: Table -> Int -> UId
getUId (Table u _) n = assoc u n

getId :: Table -> Int -> Id
getId (Table _ i) n = assoc i n

assoc :: M.IntMap a -> Int -> a
assoc m i = case M.lookup i m of Just x -> x; Nothing -> internalError ("Binary.assoc " ++ show i)

checkVersion bs =
    case splitAt (length version) bs of
    (ver, bs') | ver == version -> bs'
    (ver, _) -> internalError ("Binary version mismatch " ++ ver ++ " vs. " ++ version)

------

iIds :: IExpr -> [Id]
iIds (IUniv _ (_, t) e) = iIds t `union` iIds e
iIds (Ilam  _ (_, t) e) = iIds t `union` iIds e
iIds (IProduct its) = 
    unions (map (\ (_, (_, t, oe)) -> iIds t `union` unions (map iIds (maybeToList oe))) its)
iIds (IRecord ds) = 
    unions (map (\ (i,(t,e,_)) -> [i] `union` (iIds t `union` iIds e)) ds)
iIds (ISelect e i) = [i] `union` iIds e
iIds (ISum cs) = unions (map (unions . map (iIds . snd) . snd) cs) `union` map fst cs
iIds (ICon i t) = [i] `union` iIds t
iIds (Icase e cs d t) = 
    unions (iIds e : iIds d : iIds t : map (iIds . patToLam) cs) `union` [ i | (ICCon i, _) <- cs ]
iIds (IKind _ _) = []
iIds (ILVar v) = []
iIds (IGVar v) = [v]
iIds (IApply _ f a) = iIds f `union` iIds a
iIds (Ilet ds e) = 
    (unions [iIds e `union` iIds t | (_,(t,e)) <- ds] `union` iIds e)
iIds (Iletrec ds e) = 
    (unions [iIds e `union` iIds t | (_,(t,e)) <- ds] `union` iIds e)
iIds (ILit t _) = iIds t
iIds (IWarn _ e) = iIds e
iIds (IHasType e t) = iIds e `union` iIds t

