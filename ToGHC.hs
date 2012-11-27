module ToGHC(toGHC, hIdV, mkModName) where
import Data.List(union, findIndex, sort, (\\))
import Data.Maybe(maybeToList)
import Data.Char --(isAlphanum)
import ListUtil(mapSnd, chopList)
import Text.Printf
import Pretty
import PPrint
import Error
import Id
import ISyntax
import ISyntaxPP
import Util(unions, sortFst, assoc, traces)
import Env
import IReduce(typeOfX {-, levelOf-})
import IUtil(iGVars, patToLam)
import Position
import Rational(prRational)

toGHC :: [String] -> String -> IModule -> (String, String)
toGHC nats mn (IModule (i, (_, e, _))) =
    let datas = elimDupData [] (getDatas e)
        ldatas = map mkLdata datas
--        mmid = text (mangle (getIdString i))
        mmid = text (hIdM mn)
        mid = text (hIdV (getIdString i))
	is = iGVars e \\ [i]
        ltype (t, cs) = 
            (text "data " ~. text t ~. text " = " ~.
	     if null cs then text "X" else
            sepList (map (\ (i,n) -> foldl (~.) (text i) (replicate n (text " " ~. tANY))) cs) (text " |"))
        l = (text "module " ~. mmid ~. text "(" ~. mid ~. text ") where") ^.
            text "import PTS" ^.
	    foldr (^.) (text "") (map (\i->text "import " ~. ppIm i) is ++ map text nats) ^.
            foldr (^.) (text "") (map ltype ldatas) ^.
            (mid ~. text " :: a") ^.
            (mid ~. text " = " ~. cast ~. text " $ ") ^.
            nest 2 (pp 0 e)
    in  
	(pretty 80 60 l, init (pretty 10000 10000 mmid))

cast = text "cast"

type IdL = String
hIdT s = 'T' : mangle s
hIdC s = 'C' : mangle s
hIdV s = 'v' : mangle s
hIdM s = 'M' : mangle s
toModName (_:m) = m

mangle s = concatMap lit s
  where lit 'Z' = "ZZ"
        lit c | isAlphaNum c || c == '_' || c == '\'' = [c]
        lit c = printf "Z%02x" c

getDatas :: IExpr -> [IExpr]
getDatas (IUniv _ (i, t) e) = getDatas t `union` getDatas e
getDatas (Ilam  _ (i, t) e) = getDatas t `union` getDatas e
getDatas (IProduct its) =
    unions (map (\ (_, (_, t, oe)) -> getDatas t `union` unions (map getDatas (maybeToList oe))) its)
getDatas (IRecord ds) = 
    unions (map (\ (_,(t,e,_)) -> getDatas t `union` getDatas e) ds)
getDatas (ISelect e _) = getDatas e
getDatas e@(ISum cs) = unions (map (\(c, its) -> unions (map (getDatas . snd) its)) cs) `union` [e]
getDatas (ICon _ t) = getDatas t
getDatas (Icase e cs d t) = unions (getDatas e : getDatas d : getDatas t : map (getDatas . patToLam) cs)
getDatas (IKind _ _) = []
getDatas (ILVar v) = []
getDatas (IGVar v) = []
getDatas (IApply _ f a) = getDatas f `union` getDatas a
getDatas (Ilet [] e) = getDatas e
getDatas (Ilet ((i,(t,e)):ds) b) =
    (getDatas t `union` getDatas e) `union` getDatas (Ilet ds b)
getDatas (Iletrec ds e) = getDatas (Ilet ds e)
getDatas (ILit _ _) = []
getDatas (IWarn _ e) = getDatas e
getDatas (IHasType e t) = getDatas e `union` getDatas t

elimDupData :: [[(Id,Int)]] -> [IExpr] -> [IExpr]
elimDupData css [] = []
elimDupData css (e@(ISum cts):es) =
    let cs = mapSnd length cts in
    if cs `elem` css then
        elimDupData css es
    else
        e : elimDupData (cs:css) es

mkLdata :: IExpr -> (IdL, [(IdL, Int)])
mkLdata (ISum cts) = 
    let sign = mkTSign cts
        tId = hIdT sign
        lcs = map (\ (i, ts) -> (mkCId sign i, length ts)) cts
    in  (tId, lcs)

mkTSign :: [(Id, [a])] -> String
mkTSign cts = concatMap (\ (i,ts) -> "|" ++ getIdString i ++ "#" ++ show (length ts)) cts

mkCId sign i = hIdC (getIdString i ++ sign)

maxTuple = 16 :: Int

tANY = text "ANY"
tUNIV = text "_UNIV"
tUNIT = text "_UNIT"
tPRODUCT = text "_PRODUCT"
tSUM = text "_SUM"
tSTAR = text "_STAR"
tBOX = text "_BOX"
tUNKNOWN = text "_UNKNOWN"
tSEL k n = text (tSEL' k n)
tSEL' k n =
    let nh = (n - 1) `div` maxTuple + 1
        nl = n `mod` maxTuple
        (kh, kl) = k `divMod` maxTuple
    in  if nh == 1 then
	    "_SEL_"++show k++"_"++show n
        else
            let selH = tSEL' kh nh
                selL = tSEL' kl (if kh == nh-1 then nl else maxTuple)
            in  "(" ++ selL ++ " . " ++ selH ++ ")"

mkTuple [] = tUNIT
mkTuple xs | length xs <= maxTuple = pparen True $ sepList xs (text ",")
mkTuple xs = 
    let xss = chopList (splitAt maxTuple) xs
        ts = map (\ xs -> pparen True $ sepList xs (text ",")) xss
    in  mkTuple ts

ppIm = text . mkModName . getIdString
mkModName i = hIdM (keepLast i)
  where keepLast cs = reverse (takeWhile (/= '$') (reverse cs))
ppI i  = text (hIdV (getIdString  i))
ppUI i = 
    if isDummyUId i then
        text "_"
    else if isTmpUId i then
	text (hIdV (getUIdString i)) -- no need to suffix with unique number
    else
        text (hIdV (getUIdString i) ++ "_" ++ show (getUIdNo i))

pp _ (IUniv _ _ _) = tUNIV
pp p (Ilam _ (x,t) e) =
    pparen (p>0) $ separate [text "\\" ~. ppUI x ~. text "->", pp 0 e]
pp _ (IProduct _) = tPRODUCT
pp p (IRecord ds) = mkTuple [ pp 1 e | (_,(_,e,_)) <- sortFst ds]
pp p (IHasType es@(ISelect e i) (IProduct ds)) =
        case findIndex ((==i) . fst) ds of
        Nothing -> internalError ("toGHC: select k " ++ ppAll es ++ " " ++ ppAll (map fst ds))
        Just k -> pparen (p>9) $ tSEL k (length ds) ~. text " " ~. pp 10 e
pp _ (ISum t) = tSUM
pp _ e@(ICon i (ISum cts)) = coerce $ text (mkCId (mkTSign cts) i)
pp p (Icase (IHasType e tt@(~(ISum cts)) ) as d _) = 
        (text "case " ~. pp 0 e ~. text " of") ^.
	(text "  " ~. sepList (map f as ++ [def]) (text ""))
	where f (ICCon c,(as,e)) = separate [separate (text (mkCId scts c) : map (nest 2 . ppUI . fst) as) ~. text " ->",
					     nest 4 (pp 1 e)]
	      f (ICLit tl l,(_,e)) = separate [ppLit 0 l ~. text " ->", nest 2 (pp 1 e)]
--	      f _ = internalError "pp: unimpl literal"
	      scts = mkTSign cts
	      def = 
		case d of
		 ILit _ LImpossible -> text ""
		 _ -> text "_ -> " ~. pp 0 d
pp _ (IKind _ _) = tSTAR
pp _ (ILVar v) = coerce (ppUI v)
pp _ (IGVar v) = ppI v
pp p (IApply _ f a) = 
    pparen (p>9) $ separate [pp 9 f, nest 2 (pp 10 a)]
pp p (Ilet ds b) = pparen (p>0) $ f ds
  where f [] = pp 0 b
        f ((i,(t,e)):ds) =
	        (text "let " ~. ppUI i ~. text " = " ~. pp 0 e ~. text " in") ^.
	        f ds
pp p (Iletrec ds b) = pparen (p>0) $ 
        pparen (p > 0) $
        text "let " ^.
	(text "    " ~. sepList (map (\ (i, (_, e)) -> separate [ppUI i ~. text " =", nest 2 (pp 1 e)]) ds) (text "")) ^. 
	text "in  " ~. pp 5 b
pp p (ILit _ l) = ppLit p l
pp p (IWarn _ e) = pp p e
--pp p (IHasType e _) = pp p e
pp _ e = internalError ("toGHC: " ++ ppAll e)

ppLit _ (LChar c) = text (show c)
ppLit _ (LString s) = text (show s)
ppLit _ (LInt i) = text ("(" ++ show i ++ "::Int)")
ppLit _ (LFloat f) = text (prRational 15 f ++ "::Double")
ppLit _ (LInteger i) = text ("(" ++ show i ++ "::Integer)")
ppLit _ (LNative s) = pparen True (text s)
ppLit _ (LImpossible)= text (hIdV "%impossible")
ppLit p (LNoMatch (Position f l c)) = 
    pparen (p>9) (text (hIdV "%noMatch") ~. text " " ~. text (show f) ~. text " " ~. text (show l) ~. text " " ~. text (show c))

coerce x = pparen True (cast ~. text" " ~. x)
