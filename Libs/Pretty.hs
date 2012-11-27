module Pretty(text, separate, cseparate, nest, pretty, (~.), (^.), IText, emptyPP) where
import qualified Text.PrettyPrint.HughesPJ as P

infixr 8 ~.
infixr 8 ^.

type IText = P.Doc

text :: String -> IText
text s = P.text s

separate :: [IText] -> IText
separate xs = P.sep xs

cseparate :: [IText] -> IText
cseparate xs = P.fsep xs

nest :: Int -> IText -> IText
nest n x = P.nest n x

(~.) :: IText -> IText -> IText
x ~. y = x P.<> y

(^.) :: IText -> IText -> IText
x ^. y = x P.$+$ y

emptyPP :: IText
emptyPP = P.empty

pretty :: Int -> Int -> IText -> String
pretty w m x = P.fullRender P.PageMode w (toEnum w / toEnum m) string_txt "\n" x

string_txt (P.Chr c)   s  = c:s
string_txt (P.Str s1)  s2 = s1 ++ s2
string_txt (P.PStr s1) s2 = s1 ++ s2

