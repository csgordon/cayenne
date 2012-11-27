module ISyntaxPP(IModule(..), IExpr(..), IConstr(..), IInterface(..)) where
import Id
import PPrint
import Error(prEMsg)
import ISyntax
import CSyntax
import CITranslate

instance PPrint IInterface where
    pPrint d _ (IInterface is s) = text"interface " ~. pPrint d 0 is ~. text " " ~. pPrint d 0 s

instance PPrint IModule where
    pPrint PDDebug p (IModule d) = text "module " ~. ppdef d
    pPrint d p m = pPrint d p (iToCM (d > PDReadable) m)

instance PPrint IExpr where
    pPrint PDDebug p e = pp e
    pPrint d p e = pPrint d p (iToC (d > PDReadable) e)

ppd x = pPrint PDDebug 0 x
paren t = text "(" ~. t ~. text ")"

ppIT (i,t) = paren (ppd i ~. text " :: " ~. pp t)

instance PPrint IConstr where
    pPrint d p (ICCon i) = pPrint d p i
    pPrint d p (ICLit _ l) = ppLit l

pp (IUniv b (i,t) e) | isDummyUId i = paren $ pp t ~. text (if b then " |-> " else " -> ") ~. pp e
pp (IUniv b it e) = paren $ ppIT it ~. text (if b then " |-> " else " -> ") ~. pp e
pp (Ilam  b it e) = paren $ separate [text "\\" ~. ppIT it ~. text (if b then " |-> " else " -> "), pp e]
pp (IProduct ss) = text "sig { " ~. foldr (^.) (text "") (map f ss) ~. text " }"
  where f (i, (ui, t, Nothing)) =
          separate [ppd i ~. text"(" ~. ppd ui ~. text ")",
                    nest 2 (text ":: " ~. pp t ~. text ";")]
        f (i, (ui, t, Just e)) =
          separate [ppd i ~. text"(" ~. ppd ui ~. text ")",
                    nest 2 (text ":: " ~. pp t),
                    nest 2 (text "= " ~. pp e ~. text ";")]
pp (IRecord ds) = text "struct { " ~. foldr (^.) (text "") (map ppdef ds) ~. text " }"
pp (ISelect e i) = pp e ~. text "." ~. ppd i
pp (ISum cs) = paren $ text "data " ~. sepList (map f cs) (text "|")
  where f (c, its) = ppd c ~. text " " ~. separate (map g its)
	g (i, t) | isDummyUId i = pp t
	g it = ppIT it
pp (ICon i t) = ppd i ~. text "@" ~. pp t
pp (Icase e as d t) = paren $ (text "case " ~. pp e ~. text " of {") ^.
    foldr (^.) (text "_ -> " ~. ppd d ~. end) (map (\(c,(vs,e))->separate (ppd c:map ppIT vs) ~. text " -> " ~. ppd e ~. text ";") as)
  where end = text " }::" ~. pp t
pp (IKind n m) = text ("#"++show n++"."++show m)
pp (ILVar i) = ppd i
pp (IGVar i) = ppd i
pp (IApply h f a) = paren $ pp f ~. text (if h then " |" else " ") ~. pp a
pp (Ilet ds e) = (text "let " ~. foldr (^.) (text "") (map ppdf ds)) ^. (text "in  " ~. pp e)
pp (Iletrec ds e) = (text "letrec " ~. foldr (^.) (text "") (map ppdf ds)) ^. (text "in  " ~. pp e)
pp (ILit t (LNative s)) = paren (text("native " ++ show s ++ " ") ~. pp t)
pp (ILit _ l) = ppLit l
pp (IWarn w e) = text ("{- " ++ prEMsg w ++ " -}") ~. pp e
pp (IHasType e t) = paren $ pp e ~. text "::" ~. pp t
--pp x = error ("pp "++ ppReadable x)

ppLit :: Literal -> IText
ppLit l = pPrint PDDebug 0 l

ppdef :: IDefRec -> IText
ppdef (i, (t, e, con)) =
    separate [(if con then text "concrete " else text "abstract ") ~. ppd i,
              nest 2 (text ":: " ~. pp t),
              nest 2 (text "= " ~. pp e ~. text ";")]
ppdf :: (UId, (IType, IExpr)) -> IText
ppdf (i, (t, e)) = 
    separate [ppd i,
              nest 2 (text ":: " ~. pp t),
              nest 2 (text "= " ~. pp e ~. text ";")]
