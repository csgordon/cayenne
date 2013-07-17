module CParser(pProgram, pInterface, errSyntax) where
import Data.List(nub)
import Libs.Parse
import BinParse
import FString
import PreStrings(fsStar, fsComma, fsRArrow, fsBRArrow, fsDEF)
import Position
import Error
import CSyntax
import Id
import Lex

import Libs.Trace

infix 6 >>>> , >>>>>

type CParser a = Parser [Token] a

pProgram :: CParser (CProgram, StrTable)
pProgram = (pPackage +.+ many pModule >>> CProgram) +.+ eof

pPackage = succeed Nothing

pModule :: CParser CModule
pModule = many pNativeLine +.+ l L_module ..+ pDef pModId +.. osm >>> CModule

pNativeLine = l L_native ..+ string' +.. sm

pInterface :: CParser (CInterface, StrTable)
pInterface = (l L_interface ..+ l L_use ..+ sepBy pModId cm +.+ l L_in ..+ 
	      pSign pModId +.. sm >>> CInterface) +.+ eof

pType = pExpr

pExpr :: CParser CExpr
pExpr = expEq

expEq :: CParser CExpr
expEq = exp0 `into` \ e -> many pEqStep					>>- mkEqSteps e
  where mkEqSteps e [] = e
	mkEqSteps e xs = CEqProof (foldl (\ e f -> f e) (CEqAtom e) xs)

pEqStep :: CParser (CEqChain -> CEqChain)
pEqStep = l L_eqlcurl ..+ pDEF ..+ l L_eqrcurl ..+ exp0			>>- (\ e c -> CEqDef c e)
      ||! l L_eqlcurl ..+ exp0 +.+ l L_eqrcurl ..+ exp0			>>- (\ (p,e) c -> CEqBy c p e)

exp0 :: CParser CExpr
exp0 = binop getFixity mkBin pOper exp10

mkBin e1 op e2 = 
    let fs = getIdFString op
    in       if fs == fsRArrow  then CArrow False e1 e2
        else if fs == fsBRArrow then CArrow True  e1 e2
        else CBinOp e1 op e2

exp10 :: CParser CExpr
exp10 =     l L_let ..+ block (pDef pBindId) +.+ l L_in ..+ exp0	>>> Clet
	||! l L_open ..+ exp0 +.+ l L_use ..+ pOpenArgs +.+ l L_in ..+ exp0 >>>> Copen
        ||! l L_case ..+ exp0 +.+ l L_of ..+ block pCaseArm 		>>> Ccase
        ||! l L_data ..+ pSummands					>>- CSum
	||! l L_lam ..+ pB (many1 pPArgsT') +.+ exp0			>>> cLam
	||! l L_do ..+ aexp +.+ pDoBlock				>>> Cdo
        ||! l L_if ..+ exp0 +.+ l L_then ..+ exp0 +.+ l L_else ..+ exp0	>>>> Cif
	||! l L_native ..+ string' +.+ pType				>>> Cnative
	||! pB pPArgsT +.+ exp0						>>- (\ ((h,as), e) -> foldr CUniv e (zip (repeat h) as))
	||! aexp +.+ dc ..+ aexp					>>> CHasType
	||! aexp +.+ many abexp						>>> cApply

pB :: CParser a -> CParser (Bool, a)
pB p = p `into` \ x ->
                 rarrow							.> (False,x)
             ||! brarrow						.> (True,x)

pOpenArgs :: CParser COpenArgs
pOpenArgs = star							.> COpenAll
	||! pOpenArgs'
pOpenArgs' = sepBy pOArg cm						>>- COpenArgs

pOArg :: CParser COpenArg
pOArg = pProps +.+ pBindId +.+ option (dc ..+ pType) +.+ option (eq ..+ pBindId)	>>>>> COpenArg

pArg :: CParser CArg
pArg = pBindId `into` \ i ->
	    dc ..+ pType					 	>>- CArgT i
        ||! succeed							(CArg False i)

abexp = obar aexp

aexp :: CParser CExpr
aexp = aexp' +.+ many (l L_dot ..+ pBindId)				>>> foldl CSelect

aexp' :: CParser CExpr
aexp' =	    pId	`into` (\ i ->
		    l L_at ..+ (aexp ||! l L_uscore >>- CVar . dummyId)	>>- CCon i
                ||! succeed						(cVar i))
	||! lp ..+ pExpr +.. rp
	||! l L_lbra ..+ pExpr +.. l L_rbra				>>- CHole
	||! pTYPE 							>>>> CStar
        ||! pProps +.+ l L_struct +.+ block (pDef pBindId)		>>>> CRecord
        ||! pProps +.+ getPos +.+ hBlock (pDef pBindId)			>>>> CRecord
        ||! l L_sig +.+ block (pSign pBindId)				>>> CProduct
        ||! getPos +.+ hBlock (pSign pBindId)				>>> CProduct
	||! integer
        ||! string
        ||! char

pDoBlock :: CParser [CBind]
pDoBlock = testp "<do-block>" okBlk (block pDoBind)
  where okBlk [] = False
        okBlk bs = case last bs of CBind_ _ -> True; _ -> False

pDoBind :: CParser CBind
pDoBind =   pPArgL +.+ l L_larrow ..+ exp0				>>> CBind
        ||! l L_let ..+ block (pDef pBindId)				>>- CBLet
	||! exp0							>>- CBind_

block :: CParser a -> CParser [a]
block p = startBlock ..+ hBlock p

hBlock :: CParser a -> CParser [a]
hBlock p = lc ..+ sepBy p dsm +.. osm +.. rc 

pCaseArm :: CParser (CPat, CExpr)
pCaseArm =  pAPat +.+ rarrow ..+ pExpr

pSummands :: CParser [(Id, [CSumType])]
pSummands = sepBy pSummand (l L_bar)

pSummand :: CParser (Id, [CSumType])
pSummand = pBindId +.+ many pSumType
  where pSumType  = lp ..+ pBindId +.+ dc ..+ pType +.. rp		>>> CSId
                ||! aexp						>>- CSType

pSign :: CParser Id -> CParser CSign
pSign pi =  pDefn pi							>>- CSignDef
	||! pi +.+ dc ..+ pType					 	>>> CSign
	||! l L_type ..+ pi +.+ many pPArg				>>> CSignType

pDef :: CParser Id -> CParser CDef
pDef pi = pProps +.+ osm ..+ pDefn pi					>>> CDef

pProps :: CParser [CProp]
pProps =    l L_private							.> [Cprivate]
        ||! pub +.+ acn							>>> (++)
        ||! acn +.+ pub							>>> (++)
  where pub =     l L_public						.> [Cpublic]
              ||! succeed						   []
	acn =     l L_abstract						.> [Cabstract]
	      ||! l L_concrete						.> [Cconcrete]
              ||! succeed						   []

pDefn :: CParser Id -> CParser CDefn
pDefn pi =  (pi +.+ pDefSignType +.. dsm `into` \ (i,t) -> pClauses1 i pi >>-   CValueS i t)
        ||! pi +.+ pPBArgsTs +.+ eq ..+ pExpr				>>-  (\ (i, (as, e)) -> CValue i (foldr Clam e as))
	||! (pi +.+ many pBAPat +.+ eq ..+ pExpr `into` \ (i,(as,e)) ->
	     pClauses i pi						>>-   \ cs -> CValueP i (CClause as e : cs))
	||! pi +.+ pPBArgsTs +.+ dc ..+ pType +.+ eq ..+ pExpr		>>>>> CValueT
        ||! l L_data ..+ pi +.+ many pPArg +.+ eq ..+ pSummands		>>>>  Cdata
	||! l L_type ..+ pi +.+ many pPArg +.+ eq ..+ pType		>>>>  Ctype
	||! l L_native ..+ pi +.+ dc ..+ pType +.+ eq ..+ string'	>>>>  CNative
        ||! l L_package ..+ pi +.+ eq +.+ block (pDef pBindId)		>>- ( \ (i,(pos,ds)) -> CValue i (CRecord [Cconcrete] pos ds))
        ||! l L_postulate ..+ pi +.+ dc ..+ pType			>>>   Cpostulate
	||! l L_open ..+ pExpr +.+ l L_use ..+ pOpenArgs		>>>   CDopen
  where pPBArgsTs = many pPBArgsT >>- concatMap (\ (h,xs) -> zip (repeat h) xs)

pDefSignType :: CParser CType
pDefSignType = many arg +.+ dc ..+ pType				>>> flip (foldr ($))
  where arg :: CParser (CExpr -> CExpr)
        arg = (    obar pPArgT	>>- CUniv
               ||! obar aexp	>>> CArrow)

pClauses :: Id -> CParser Id -> CParser [CClause]
pClauses i pi = many (dsm ..+ pClause i pi)

pClauses1 :: Id -> CParser Id -> CParser [CClause]
pClauses1 i pi = sepBy1 (pClause i pi) dsm

pClause :: Id -> CParser Id -> CParser CClause
pClause i pi = piEq i pi ..+ many pBAPat +.+ eq ..+ pExpr		>>> CClause

piEq :: Id -> CParser Id -> CParser Id
piEq i pi = testp (getIdString i) (\i'->i==i') pi

pPatApply :: CParser CPat
pPatApply = pBindId +.+ many pAPat					>>> CPCon

pPatOp :: CParser CPat
pPatOp = binop getFixity mkBinP pOper pAPat
  where mkBinP p1 op p2 = CPCon op [p1, p2]

pBAPat :: CParser (Bool, CPat)
pBAPat = obar pAPat

pAPat :: CParser CPat
pAPat =     pBindId `into` (\ i ->
                                l L_at ..+ pAPat			>>- CPAs (CArg False i)
                            ||! succeed					    (CPVar (CArg False i)))
	||! lp ..+ pPatApply +.. rp
	||! lp ..+ pPatOp +.. rp
        ||! char							>>- (\ (CLit p l) -> CPLit p l)
        ||! integer							>>- (\ (CLit p l) -> CPLit p l)

pPBArgsT :: CParser (Bool, [CArg])
pPBArgsT = obar pPArgsT

obar :: CParser a -> CParser (Bool, a)
obar p = l L_bar ..+ p							>>- (\x->(True,x))
         ||! p								>>- (\x->(False,x))

pPArg :: CParser CArg
pPArg =     pBindId							>>- CArg False
	||! pPArgT

pPArgL :: CParser CArg
pPArgL =    pBindId							>>- CArg True
	||! pPArgT

pPArgT :: CParser CArg
pPArgT = lp ..+ pBindId +.+ dc ..+ pType +.. rp			 	>>> CArgT

pPArgsT' :: CParser [CArg]
pPArgsT' = many1 pBindId						>>- map (CArg True)
       ||! pPArgsT

pPArgsT :: CParser [CArg]
pPArgsT = lp ..+ sepBy1 pBindId cm +.+ dc ..+ pType +.. rp	 	>>- (\ (is, t) -> [CArgT i t | i <- is])

pOper :: CParser Id
pOper = pOpId ||! l L_bquote ..+ pVarId +.. l L_bquote

pModId, pVarId, pBindId, pOpId :: CParser Id
pModId = lcp "<modid>" (\p x->case x of L_modid fs -> Just (mkId p fs); _ -> Nothing)

pBindId = pVarId
      ||! lp ..+ pOpId +.. rp
      ||! l L_uscore >>- dummyId

pVarId = lcp "<id>" (\p x->case x of L_varid fs -> Just (mkId p fs); _ -> Nothing)

pOpId = lcp "<op>" (\p x->case x of L_varsym fs -> Just (mkId p fs); _ -> Nothing)

pId :: CParser Id
pId = pModId ||! pBindId

-- Utilities

p >>>> f = p >>- \ (x,(y,z)) -> f x y z
p >>>>> f = p >>- \ (x,(y,(z,w))) -> f x y z w

option p = p >>- Just ||! succeed Nothing

eq = l L_eq
lp = l L_lpar
rp = l L_rpar
--cm = l L_comma
lc = l L_lcurl
rc = l L_rcurl
sm = l L_semi
dc = l L_dcolon
osm = sm ||! succeed noPosition
dsm = sm +.. osm
eof = lcp "<EOF>" (\p x->case x of L_eof x -> Just x; _ -> Nothing)

l :: LexItem -> CParser Position
l li = 	token ( \ls->
	case ls of
	Token p li' : ls' -> if li==li' then Right (p, ls') else Left (prLexItem li) )

getPos :: CParser Position
getPos = token ( \ls->
	case ls of
	Token p _ : _ -> Right (p, ls))

lcp :: String -> (Position -> LexItem -> Maybe a) -> CParser a
lcp s f = 
	token $ \ls-> 
	case ls of
	Token p li : ls' -> 
	    case f p li of
	    Just x  -> Right (x, ls')
	    Nothing -> Left s

startBlock :: CParser ()
startBlock =
	token $ \ ts ->
        case ts of
        t@(Token p@(Position _ _ c) li) : ts' | li /= L_lcurl ->
            Right ((), Token p L_lcurl : t : col c ts')
        _ -> Right ((), ts)
  where col c (t@(Token p@(Position _ _ c') _) : ts) | c' == c = Token p L_semi : t : col c ts
        col c (t@(Token p@(Position _ _ c') _) : ts) | c' >  c = t : col c ts
        col c (t@(Token p@(Position _ _ c') _) : ts) | c' <  c = Token p L_rcurl : t : ts
        col c [] = [Token noPosition L_rcurl] -- XXX bad position

errSyntax :: [String] -> [Token] -> EMsg
errSyntax ss ts =
    	case ts of
	Token p (L_error em) : _ -> (p, em)
	Token p li           : _ -> (p, ESyntax (showt (prLexItem li)) (map showt (nub ss)))
		where showt t = case show t of
				    "\"\\\\\"" -> "\"\\\""
				    s -> s

pTYPE   = lcp "<kind>" (\p x->case x of L_star nn -> Just (p, nn); _ -> Nothing)

integer = lcp "<integer>" (\p x->case x of L_integer i-> Just (CLit p (LInteger i)); _ -> Nothing)
string  = lcp "<string>"  (\p x->case x of L_string  s-> Just (CLit p (LString  s)); _ -> Nothing)
char    = lcp "<char>"    (\p x->case x of L_char    c-> Just (CLit p (LChar    c)); _ -> Nothing)

string' = string >>- \ (CLit _ (LString s)) -> s

star    = lcp "*"   (\p x->case x of L_varsym fs | fs == fsStar    -> Just p; _ -> Nothing)
cm      = lcp ","   (\p x->case x of L_varsym fs | fs == fsComma   -> Just p; _ -> Nothing)
rarrow  = lcp "->"  (\p x->case x of L_varsym fs | fs == fsRArrow  -> Just p; _ -> Nothing)
brarrow = lcp "|->" (\p x->case x of L_varsym fs | fs == fsBRArrow -> Just p; _ -> Nothing)

pDEF = lcp "DEF" (\p x->case x of L_varid fs | fs == fsDEF -> Just p; _ -> Nothing)

cLam :: (Bool, [[CArg]]) -> CExpr -> CExpr
cLam (h, vss) e = foldr Clam e (zip (repeat h) (concat vss))
