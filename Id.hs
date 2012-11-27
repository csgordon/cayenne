module Id(Id, mkId, dummyId, getIdString, getIdPosition, toId, isDummyId, getIdFString, getIdNo,
          UId, mkUId, dummyUId, getUIdString, getUIdPosition, toUId, sameId, isDummyUId, getUIdNo, getUIdFString,
	  isModuleId, isModuleString, getFixity, mkTmpUId, isTmpUId, arrowPrec
         ) where
import PPrint
import FString
import Position
import PreStrings
import BinParse(Fixity(..))

data Id = Id Position FString

instance Eq Id where
	Id _ fs == Id _ fs'  =  fs == fs'

instance Ord Id where
	Id _ fs <= Id _ fs'  =  getFString fs <= getFString fs'

mkId pos fs = Id pos fs

dummyId p = Id p fsUnderscore
isDummyId (Id _ fs) = fs == fsUnderscore

instance PPrint Id where
    pPrint _ _ (Id _ fs) = text (getFString fs)

getIdString :: Id -> String
getIdString (Id _ fs) = getFString fs

getIdPosition :: Id -> Position
getIdPosition (Id p _) = p

getIdFString :: Id -> FString
getIdFString (Id _ fs) = fs

getIdNo :: Id -> Int
getIdNo (Id _ fs) = getFStrNo fs

data UId = UId Position FString Int

instance Eq UId where
	UId _ _ i == UId _ _ i'  =  i == i'

instance Ord UId where
	UId _ fs _ <= UId _ fs' _ =  getFString fs <= getFString fs'

mkUId pos fs u = UId pos fs u

dummyUId pos = UId pos fsUnderscore (-1)

isDummyUId (UId _ _ n) = n == -1

instance PPrint UId where
    pPrint d _ (UId _ fs u) = 
        if d == PDDebug then
            text (getFString fs ++ "#" ++ show u)
        else
            text (getFString fs)

getUIdString :: UId -> String
getUIdString (UId _ fs _) = getFString fs

getUIdFString :: UId -> FString
getUIdFString (UId _ fs _) = fs

getUIdPosition :: UId -> Position
getUIdPosition (UId p _ _) = p

getUIdNo :: UId -> Int
getUIdNo (UId _ _ n) = n

toId (UId p fs _) = Id p fs

toUId (Id p fs) u = UId p fs u

mkTmpUId :: Int -> UId
mkTmpUId u = UId noPosition (tmpFString u ("_"++show u)) u

isTmpUId :: UId -> Bool
isTmpUId (UId _ fs _) = isTmpFString fs

sameId :: Id -> UId -> Bool
sameId (Id _ fs) (UId _ fs' _) = fs == fs'

isModuleId i = isModuleString (getIdString i)
isModuleString s = '$' `elem` s

getFixity :: Id -> Fixity
getFixity i =
    case getIdString i of
    "->"  -> FInfixr 0
    "|->" -> FInfixr 0
    "===" -> FInfix  1
    ","   -> FInfixr 1
    "||"  -> FInfixr 2
    "&&"  -> FInfixr 3
    "=="  -> FInfix  4
    "/="  -> FInfix  4
    "<="  -> FInfix  4
    ">="  -> FInfix  4
    "<"   -> FInfix  4
    ">"   -> FInfix  4
    ":"   -> FInfixr 5
    "++"  -> FInfixr 5
    "+"   -> FInfixl 6
    "-"   -> FInfixl 6
    "*"   -> FInfixl 7
    "/"   -> FInfixl 7
    "·"   -> FInfixr 8
    _     -> FInfixl 9

arrowPrec :: Int
arrowPrec = 0
