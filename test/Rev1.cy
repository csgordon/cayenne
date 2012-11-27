-- compile with
-- cayenne -steps 3000000 Rev.cy
--

module example$Rev = 
open System$List use * in
open System$Id use (===), symmH in

struct

concrete
(++) :: (a :: #) |-> List a -> List a -> List a
(++) (Nil) ys = ys
(++) (x : xs) ys = x : (xs ++ ys)

appendNilP :: (a :: #) |-> (xs :: List a) -> 
	xs ++ Nil === xs
appendNilP |a (Nil) =
	Nil|a ++ Nil			={ DEF }=
	Nil
appendNilP |a (x : xs) =
	(x:xs) ++ Nil			={ DEF }=
	x:(xs ++ Nil)			={ appendNilP xs }=
	x:xs				={ DEF }=
	Nil ++ (x:xs)

appendAssocP :: (a :: #) |-> (xs, ys, zs :: List a) -> 
	(xs++ys)++zs === xs++(ys++zs)
appendAssocP |a (Nil) ys zs =
	(Nil|a ++ ys) ++ zs		={ DEF }=
	ys ++ zs			={ DEF }=
	Nil ++ (ys ++ zs)
appendAssocP |a (x:xs) ys zs =
	((x : xs) ++ ys) ++ zs		={ DEF }=
	(x : (xs ++ ys)) ++ zs		={ DEF }=
	x : ((xs ++ ys) ++ zs)		={ appendAssocP xs ys zs }=
	x : (xs ++ (ys ++ zs))		={ DEF }=
	(x : xs) ++ (ys ++ zs)

-----

concrete
rev :: (a :: #) |-> List a -> List a
rev (Nil) = Nil
rev (x : xs) = rev xs ++ (x : Nil)

revAppendP :: (a :: #) |-> (xs, ys :: List a) -> 
	rev (xs++ys) === rev ys ++ rev xs
revAppendP |a (Nil) ys =
	rev (Nil|a ++ ys)		={ DEF }=
	rev ys				={ symmH (appendNilP (rev ys)) }=
	rev ys ++ Nil			={ DEF }=
	rev ys ++ rev (Nil|a)
revAppendP |a (x:xs) ys =
	rev ((x : xs) ++ ys)		={ DEF }=
	rev (x : (xs ++ ys))		={ DEF }=
	rev (xs ++ ys) ++ (x:Nil)	={ revAppendP xs ys }=
	(rev ys ++ rev xs) ++ (x:Nil)	={ appendAssocP (rev ys) (rev xs) (x:Nil) }=
	rev ys ++ (rev xs ++ (x:Nil))	={ DEF }=
	rev ys ++ (rev (x:xs))

revRevP :: (a :: #) |-> (xs :: List a) -> 
	rev (rev xs) === xs
revRevP |a (Nil) =
	rev (rev (Nil|a))		={ DEF }=
	rev (Nil|a)			={ DEF }=
	Nil
revRevP |a (x:xs) =
	rev (rev (x:xs))		={ DEF }=
	rev (rev xs ++ (x:Nil))		={ revAppendP (rev xs) (x:Nil) }=
	rev (x:Nil) ++ rev (rev xs)	={ revRevP xs }=
	rev (x:Nil) ++ xs		={ DEF }=
	(x:Nil) ++ xs			={ DEF }=
	x:xs
