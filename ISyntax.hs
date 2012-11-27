module ISyntax(IModule(..), IInterface(..), IExpr(..), IType, IDef, IConstr(..),
        IDefRec, IArg, ISign, Literal(..), ICaseArm,
	iStar, iArrow, iApply, iNative) where
import PPrint
import Error
import PreStrings
import Position
import Id
import Literal

data IModule
	= IModule IDefRec
	deriving (Eq, Ord)

data IInterface
	= IInterface [Id] ISign
	deriving (Eq, Ord)

data IExpr
	= IUniv Bool IArg IExpr
        | Ilam Bool IArg IExpr
        | IProduct [ISign]
        | IRecord [IDefRec]
	| ISelect IExpr Id
        | ISum ISummands
        | ICon Id IType
        | Icase IExpr [ICaseArm] IExpr IType
        | IKind Int Int
        | ILVar UId
        | IGVar Id
        | IApply Bool IExpr IExpr
	| Ilet [IDef] IExpr
	| Iletrec [IDef] IExpr
        | ILit IType Literal
	| IWarn EMsg IExpr
	| IHasType IExpr IType
        deriving (Eq, Ord)

data IConstr
	= ICCon Id
        | ICLit IType Literal
--        deriving (Eq, Ord)
instance Eq IConstr where
    ICCon i   == ICCon i'    =  i == i'
    ICLit _ l == ICLit _ l'  =  l == l'
    _         == _           =  False
instance Ord IConstr where
    ICCon i   <= ICCon i'    =  i <= i'
    ICLit _ l <= ICLit _ l'  =  l <= l'
    ICCon _   <= ICLit _ _   =  True
    _         <= _           =  False

type IType = IExpr
type ISummands = [(Id, [(UId, IType)])]
type IArgs = [IArg]
type ICaseArm = (IConstr, ([IArg], IExpr))
type IDef = (UId, (IType, IExpr))
type IDefRec = (Id, (IType, IExpr, Bool)) -- concrete
type IArg = (UId, IType)
type ISign = (Id, (UId, IType, Maybe IExpr))

iArrow a b = IUniv False (dummyUId noPosition, a) b -- XXX

iApply f a = IApply False f a

iStar = IKind 0 0

iNative :: String -> IType -> IExpr
iNative s t = ILit t (LNative s)
