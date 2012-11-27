module PPrint(PPrint(..), module Pretty, PDetail(..),
	ppReadable, ppAll, ppDebug, ppString,
	pparen, sepList) where
import Pretty

data PDetail = PDReadable | PDAll | PDDebug deriving (Eq, Ord)

class PPrint a where
    pPrint :: PDetail -> Int -> a -> IText
--    pPrint _ _ x = text ("***"++show x++"***")

ppReadable :: (PPrint a) => a -> String
ppReadable = ppr PDReadable

ppAll :: (PPrint a) => a -> String
ppAll = ppr PDAll

ppDebug :: (PPrint a) => a -> String
ppDebug = ppr PDDebug

lineWidth = 100::Int
linePref = 60::Int

ppString :: (PPrint a) => a -> String
ppString = init . ppReadable

ppr d = pretty lineWidth linePref . pPrint d 0

instance PPrint Int where
    pPrint _ _ x = text (show x)

instance PPrint Integer where
    pPrint _ _ x = text (show x)

instance PPrint Bool where
    pPrint _ _ x = text (show x)

instance PPrint Char where
    pPrint _ _ x = text (show x)

instance (PPrint a, PPrint b) => PPrint (a, b) where
    pPrint d _ (x, y) = text "(" ~. separate [pPrint d 0 x ~. text ", ", pPrint d 0 y] ~. text ")"

instance (PPrint a, PPrint b, PPrint c) => PPrint (a, b, c) where
    pPrint d _ (x, y, z) = text "(" ~. separate [pPrint d 0 x ~. text ", ", pPrint d 0 y ~. text ", ", pPrint d 0 z] ~. text ")"

instance (PPrint a, PPrint b, PPrint c, PPrint d) => PPrint (a, b, c, d) where
    pPrint d _ (x, y, z, w) = text "(" ~. separate [pPrint d 0 x ~. text ", ", pPrint d 0 y ~. text ", ", pPrint d 0 z ~. text ", ", pPrint d 0 w] ~. text ")"

instance (PPrint a) => PPrint [a] where
    pPrint d _ [] = text "[]"
    pPrint d _ xs = let (y:ys) = reverse (map (pPrint d 0) xs)
			ys' = map (~. text ",") ys
			xs' = reverse (y:ys')
--		    in  text "[" ~. cseparate xs' ~. text "]"
		    in  text "[" ~. separate xs' ~. text "]"

instance (PPrint a, PPrint b) => PPrint (Either a b) where
    pPrint d p (Left x) = pparen (p>9) (text"(Left " ~. pPrint d 10 x ~. text")")
    pPrint d p (Right x) = pparen (p>9) (text"(Right " ~. pPrint d 10 x ~. text")")

instance (PPrint a) => PPrint (Maybe a) where
    pPrint _ _ Nothing = text"Nothing"
    pPrint d p (Just x) = pparen (p>9) (text"Just (" ~. pPrint d 10 x ~. text")")

pparen False x = x
pparen True  x = text"(" ~. x ~. text")"

sepList [] _ = text ""
sepList xs s = separate (map (\x->x ~. s) (init xs) ++ [last xs])

