module CSyntax(CProgram(..), CModule(..), CInterface(..), CExpr(..), CType, CSumType(..),
        COpenArg(..), CClause(..), CPat(..), CBind(..), COpenArgs(..), Literal(..), CEqChain(..),
	CDefn(..), CProp(..), CDef(..), CSign(..), CArg(..), cApply, cVar, CArgB) where
import Lex(isIdChar)
import BinParse(Fixity(..))
import PPrint
import Error
import Position
import Id
import Literal

import Libs.Trace

data CProgram
	= CProgram (Maybe String) [CModule]
	| ErrProgram EMsg
	deriving (Eq, Ord)

instance PPrint CProgram where
    pPrint d _ (CProgram Nothing ms) = foldr (^.) (t"") (map (pp d) ms)
    pPrint d _ (CProgram (Just s) ms) = 
        (t"package " ~. t s ~. t";") ^.
        foldr (^.) (t"") (map (pp d) ms)
    pPrint d _ (ErrProgram m) = pre m

data CModule
	= CModule [String] CDef
	deriving (Eq, Ord)

instance PPrint CModule where
    pPrint d _ (CModule ns def) = 
        foldr (^.) (t"module" ^. pp d def)
		   (map (\s->t"native " ~. t(show s) ~. t";") ns)

data CInterface
	= CInterface [Id] CSign
	| ErrInterface EMsg
	deriving (Eq, Ord)

instance PPrint CInterface where
    pPrint d _ (CInterface is s) =
	(t"interface " ~. t"use " ~. sepList (map (pp d) is) (t",")) ^. t"in" ^.
	case s of
	CSign i ty -> (ppId d i ~. t" ::") ^. pp d ty ~. t";"
        CSignDef (Ctype i as ty) ->
	    (separate ((t"type " ~. ppId d i) : map (pPrint d 10) as) ~. 
             t" = ") ^. pp d ty
            ~. t";"
	_ -> pp d s
    pPrint d _ (ErrInterface m) = pre m

data CExpr
	= CUniv CArgB CExpr
        | Clam CArgB CExpr
        | CArrow Bool CType CType
        | Clet [CDef] CExpr
        | CProduct Position [CSign]
        | CRecord [CProp] Position [CDef]
        | Copen CExpr COpenArgs CExpr
	| CSelect CExpr Id
        | CSum CSummands
        | CCon Id CType
        | Ccase CExpr CCaseArms
        | CStar Position Int Int
        | CVar Id
        | CApply CExpr [(Bool,CExpr)]
        | CLit Position Literal
	| CBinOp CExpr Id CExpr
	| Cdo CExpr [CBind]
        | CWarn EMsg CExpr
	| CHasType CExpr CType
        | Cif CExpr CExpr CExpr
	| Cnative String CExpr
	| CEqProof CEqChain
	| CHole CExpr
        deriving (Eq, Ord)

data CEqChain
	= CEqAtom CExpr
	| CEqDef CEqChain CExpr
	| CEqBy CEqChain CExpr CExpr
        deriving (Eq, Ord)

cApply e [] = e
cApply (CApply e []) as = CApply e as
cApply e as = CApply e as

cVar v = CApply (CVar v) []

type CType = CExpr

type CSummands = [(Id, [CSumType])]
data CSumType 
	= CSId Id CType 
	| CSType CType 
	deriving (Eq, Ord)

type CArgs = [CArg]
type CCaseArms = [(CPat, CExpr)]

ppOp d pd i p1 p2 =
	let (p, lp, rp) =
		case getFixity i of 
		FInfixl p -> (p, p, p+1)
		FInfixr p -> (p, p+1, p)
		FInfix  p -> (p, p+1, p+1)
	in pparen (d > PDReadable || pd>p) 
                  (pPrint d lp p1 ~. t" " ~. ppInfix d i ~. t" " ~. pPrint d rp p2)

instance PPrint CExpr where
    pPrint d p ty@(CUniv a e) = pparen (p > arrowPrec) (ppArrows d ty [])
    pPrint d p (Clam  a e) = ppQuant "\\ "  d p a e
    pPrint d p ty@(CArrow b a r) = pparen (p > arrowPrec) (ppArrows d ty [])
    pPrint d p (Clet [] e) = pparen (p > 0) $
	(t"let in " ~. pp d e)
    pPrint d p (Clet ds e) = pparen (p > 0) $
	(t"let " ~. foldr1 (^.) (map (pp d) ds)) ^.
        (t"in  " ~. pp d e)
    pPrint d p (CProduct _ as) = pparen (p > 0) $
        separate [t"sig {", nest 2 (separate (map (pp d) as)), t"}"]
    pPrint d p (CRecord ps _ ds) = pparen (p > 0) $
        separate (t"struct " ~. separate (map (pp d) ps) ~. t"{" : [nest 2 (separate (map (pp d) ds)), t"}"])
    pPrint d p (Copen e as b) =
        (t"open " ~. pp d e ~. t" use " ~. pp d as ~. t" in") ^. 
        (pp d b)
    pPrint d p (CSelect e i) = pparen (p > 12) $ pPrint d 12 e ~. t"." ~. ppId d i
    pPrint d p (CSum cs) = pparen (p > 0) $
	t"data " ~. ppSummands d cs
    pPrint d p (CCon i ty) = 
	pparen (p > 12) $ ppId d i ~. t"@" ~. pPrint d 12 ty
    pPrint d p (Ccase e arms) = pparen (p > 0) $ ppCase d e arms
    pPrint d p (CStar _ n m) = t("#" ++ (if n > 0 then show n else "") ++ if n /= m then "."++show m else "")
    pPrint d p (CVar i) = ppId d i
    pPrint PDReadable p (CApply e []) = pPrint PDReadable p e
    pPrint d p (CApply e es) = pparen (p>9) $
	separate (pPrint d 10 e : map (nest 2 . ppApArg) es)
        where ppApArg (False, e) = pPrint d 10 e
              ppApArg (True,  e) = text "|" ~. pPrint d 10 e
    pPrint d p (CLit _ l) = pPrint d p l
    pPrint d p (CBinOp e1 i e2) = ppOp d p i e1 e2
    pPrint d p (Cdo e bs) =
        pparen (p>0) $ 
	t"do " ~. pPrint d 11 e ~. t" " ~. 
	separate [t"{", nest 2 (separate (map (pp d) bs)), t"}"]
    pPrint d p (CWarn _ e) = pPrint d p e
    pPrint d p (CHasType e t) = pparen (p>0) $ pPrint d 10 e ~. text "::" ~. pPrint d 10 t
    pPrint d p (Cif c tr e) = pparen (p>0) (separate [t"if " ~. pp d c ~. t" then", nest 4 (pp d tr), t"else", nest 4 (pp d e)])
    pPrint d p (Cnative s e) = pparen (p>0) (separate [t"native", t(show s), pPrint d 10 e])
    pPrint d p (CEqProof c) = pparen (p>0) (pPrint d 0 c)
    pPrint d p (CHole e) = t"[" ~. pPrint d 0 e ~. t"]"

instance PPrint CEqChain where
    pPrint d _ (CEqAtom e) = pPrint d 0 e
    pPrint d _ (CEqDef c e) = (pPrint d 0 c ~. text " ={ DEF }") ^. pPrint d 0 e
    pPrint d _ (CEqBy c p e) = (pPrint d 0 c ~. text " ={ " ~. pPrint d 0 p ~. text " }") ^. pPrint d 0 e

ppArrows :: PDetail -> CType -> [IText] -> IText
ppArrows d (CArrow b a r) ps = ppArrows d r (pPrint d arrowPrec1 a ~. t(if b then " |->" else " ->") : ps)
ppArrows d (CUniv (b,a) r) ps = ppArrows d r (pPrint d arrowPrec1 a ~. t(if b then " |->" else " ->") : ps)
ppArrows d ty ps = separate (reverse (pPrint d arrowPrec ty : ps))

ppQuant s d p (b,a) e = 
    pparen (p>0) (separate [t s ~. pPrint d arrowPrec1 a ~. t(if b then " |->" else " ->"), pp d e])

arrowPrec1 = arrowPrec+1

ppCase d e arms =
    (t"case " ~. pp d e ~. t" of {") ^.
    {-nest 4-} (separate (map (\ (p, e) -> separate [pPrint d 10 p ~. t" -> ", nest 2 (pp d e ~. t";")]) arms)) ^.
    t"}"

data CProp 
	= Cprivate 
	| Cpublic 
	| Cabstract 
	| Cconcrete
	deriving (Eq, Ord, Show)

instance PPrint CProp where
    pPrint _ _ p = t (tail (show p))

data COpenArgs
	= COpenArgs [COpenArg]
	| COpenAll
	deriving (Eq, Ord)

data COpenArg
	= COpenArg [CProp] Id (Maybe CType) (Maybe Id)
	deriving (Eq, Ord)

instance PPrint COpenArgs where
    pPrint d p (COpenArgs as) = sepList (map (pp d) as) (t",")
    pPrint d p COpenAll = t"*"

instance PPrint COpenArg where
    pPrint d p (COpenArg ps i Nothing   Nothing)   = ppProps d ps ~. ppId d i
    pPrint d p (COpenArg ps i Nothing   (Just oi)) = ppProps d ps ~. ppId d i ~. t" :: " ~. ppId d oi
    pPrint d p (COpenArg ps i (Just ty) Nothing)   = pparen (p > 0) (ppProps d ps ~. ppId d i ~. t" :: " ~. pPrint d 6 ty)
    pPrint d p (COpenArg ps i (Just ty) (Just oi)) = pparen (p > 0) (ppProps d ps ~. ppId d i ~. t" = " ~. ppId d oi ~. t" :: " ~. pPrint d 6 ty)

data CDef = CDef [CProp] CDefn
        deriving (Eq, Ord)

instance PPrint CDef where
    pPrint d p (CDef ps def) = ppProps d ps ~. pp d def

ppProps d ps = foldr (~.) (t"") (map ((~. t" ") . pp d) ps)

data CDefn
	= CValueT Id [CArgB] CType CExpr
	| CValueS Id CType [CClause]
	| CValueP Id [CClause]
        | Ctype Id CArgs CType
        | Cdata Id CArgs CSummands
	| CValue Id CExpr
	| CNative Id CType String
	| CDSign Id CType
	| Cpostulate Id CType
        | CDopen CExpr COpenArgs
        deriving (Eq, Ord)

instance PPrint CDefn where
    pPrint d p (CValueT i as ty e) =
	separate [separate [separate (ppId d i : map ppBA as) ~. t" ::",
			    nest 2 (pp d ty ~. t" =")],
		  nest 2 (pp d e)] ~. t";"
      where ppBA (False,a) = pPrint d 10 a
            ppBA (True,a) = t"|" ~. pPrint d 10 a
    pPrint d p (CValueS i ty cs) =
	separate [ppId d i ~. t" ::", nest 4 (pp d ty ~. t";")] ^.
	foldr1 (^.) (map (\ cl -> ppClause d p [ppId d i] cl ~. t";") cs)
    pPrint d p (CValueP i cs) =
	foldr1 (^.) (map (\ cl -> ppClause d p [ppId d i] cl ~. t";") cs)
    pPrint d p (Ctype i as ty) =
	separate [separate ((t"type " ~. ppId d i) : map (nest 2 . pPrint d 10) as) ~. t" =",
		  nest 2 (pp d ty ~. t";")]
    pPrint d p (Cdata i as cs) =
	separate [separate ((t"data " ~. ppId d i) : map (nest 2 . pPrint d 10) as) ~. t" =",
		  nest 2 (ppSummands d cs ~. t";")]
    pPrint d p (CValue i e) =
	separate [ppId d i ~. t" =",
		  nest 2 (pp d e)] ~. t";"
    pPrint d p (CNative i ty s) =
	t"native " ~. ppId d i ~. t" :: " ~. pp d ty ~. t" = " ~. t (show s) ~. t";"
    pPrint d p (CDSign i ty) =
	separate [ppId d i ~. t" ::", nest 2 (pp d ty ~. t";")]
    pPrint d p (Cpostulate i ty) =
	separate [t"postulate " ~. ppId d i ~. t" ::", nest 2 (pp d ty ~. t";")]
    pPrint d p (CDopen e as) =
        t"open " ~. pp d e ~. t" use " ~. pp d as ~. t";"

ppSummands d cs = sepList (map (nest 2 . ppCon) cs) (t" |")
  where ppCon (i, ts) = separate (ppId d i : map (nest 2 . pPrint d 10) ts)
--ppSummands d cs = text ("ppSummands") -- ++ show (length cs))
--	--separate (map (pp d) cs)

instance PPrint CSumType where
    pPrint d p (CSId i ty) = pparen True (ppId d i ~. t" :: " ~. pp d ty)
    pPrint d p (CSType ty) = pPrint d p ty

data CClause 
	= CClause [(Bool,CPat)] CExpr
        deriving (Eq, Ord)

instance PPrint CClause where
    pPrint d p cl = ppClause d p [] cl

ppClause d p xs (CClause ps e) = separate [separate (xs ++ map f ps) ~. t" =", nest 4 (pp d e)]
	where f (False, p) = pPrint d 10 p
	      f (True,  p) = t"|" ~. pPrint d 10 p

data CPat
	= CPCon Id [CPat]
        | CPVar CArg
        | CPAs CArg CPat
        | CPLit Position Literal
        deriving (Eq, Ord)

instance PPrint CPat where
    pPrint d p (CPVar a) = pPrint d p a
    pPrint d p (CPCon i as) = pparen (p>9) $ separate (ppId d i : map (pPrint d 10) as)
    pPrint d p (CPAs a pp) = pPrint d 10 a ~. t"@" ~. pPrint d 10 pp
    pPrint d p (CPLit _ l) = pPrint d p l

data CArg
	= CArg Bool Id
        | CArgT Id CType
        deriving (Eq, Ord)

type CArgB = (Bool, CArg)

instance PPrint CArg where
    pPrint d p (CArg _ i) = ppId d i
    pPrint d p (CArgT i ty) = pparen (p > 0) (ppId d i ~. t" :: " ~. pPrint d 6 ty)
 
data CSign
	= CSign Id CType
	| CSignDef CDefn
	| CSignType Id CArgs
        deriving (Eq, Ord)

instance PPrint CSign where
    pPrint d p (CSign i ty) = pPrint d 0 (CDSign i ty)
    pPrint d p (CSignDef def) = pPrint d 0 def
    pPrint d p (CSignType i as) =
	separate ((t"type " ~. ppId d i) : map (nest 2 . pPrint d 10) as) ~. t";"
 
data CBind
	= CBind CArg CExpr
	| CBind_ CExpr
	| CBLet [CDef]
        deriving (Eq, Ord)

instance PPrint CBind where
    pPrint d p (CBind a e) = pp d a ~. t" <- " ~. pp d e ~. t";"
    pPrint d p (CBind_ e) = pp d e ~. t";"
    pPrint d p (CBLet ds) = t"let " ~. foldr1 (^.) (map (pp d) ds) ~. t";"

ppId :: PDetail -> Id -> IText
ppId d i = 
    case getIdString i of
    s@(c:_) | isIdChar c -> t s
    s -> t ("("++s++")")

ppInfix :: PDetail -> Id -> IText
ppInfix d i =
    case getIdString i of
    s@(c:_) | isIdChar c -> t"`" ~. t s ~. t"`"
    s -> t s

-- Utilities

pp :: (PPrint a) => PDetail -> a -> IText
pp d x = pPrint d 0 x

t s = text s

pre m = t ("ERROR:\n"++prEMsg m)

