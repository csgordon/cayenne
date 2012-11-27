-- compile with
-- cayenne -steps 3000000 Rev.cy
--

module example$Rev = 
open System$List use * in
open System$Id use * in

struct

cons :: (a :: #) |-> a -> List a -> List a
cons x xs = x : xs

snoc :: (a :: #) |-> a -> List a -> List a
snoc x xs = xs ++ (x:Nil)

------

concrete
(++) :: (a :: #) |-> List a -> List a -> List a
(++) (Nil) ys = ys
(++) (x : xs) ys = x : (xs ++ ys)

appendNilP :: (a :: #) |-> (xs :: List a) -> 
	xs ++ Nil === xs
appendNilP |a (Nil) = 
	refl |(List a) |(Nil | a)
appendNilP |a (x : xs) =
	subst (cons x) (appendNilP xs)

appendAssocP :: (a :: #) |-> (xs, ys, zs :: List a) -> 
	(xs++ys)++zs === xs++(ys++zs)
appendAssocP (Nil) ys zs = 
	refl |(List a) |(ys++zs)
appendAssocP (x:xs) ys zs =
	subst (cons x) (appendAssocP xs ys zs)
-----

concrete
rev :: (a :: #) |-> List a -> List a
rev (Nil) = Nil
rev (x : xs) = rev xs ++ (x : Nil)

revAppendP :: (a :: #) |-> (xs, ys :: List a) -> 
	rev (xs++ys) === rev ys ++ rev xs
revAppendP |a (Nil) ys = 
	symm (appendNilP (rev ys))
revAppendP |a (x:xs) ys =
	let e1 :: rev ((x:xs) ++ ys) === (rev ys ++ rev xs) ++ (x:Nil)
{-
	    -- This is what I want, but I guess the type checker is buggy...
	    e1 = subst (snoc x)	(revAppendP xs ys)
-}
	    e1 = congr	|(List a)
			(\ (zs :: List a) -> (rev(xs++ys)++(x:Nil)) === (zs++(x:Nil)))
			(rev(xs++ys))
			(rev ys ++ rev xs)
			(revAppendP xs ys)
			(refl (rev(xs++ys)++(x:Nil)))

	    e2 :: (rev ys ++ rev xs) ++ (x:Nil) ===
		   rev ys ++ (rev xs ++ (x:Nil))
	    e2 = appendAssocP (rev ys) (rev xs) (x:Nil)
	in  transH e1 e2

revRevP :: (a :: #) |-> (xs :: List a) -> 
	rev (rev xs) === xs
revRevP |a (Nil) = 
	refl (Nil | a)
revRevP |a (x:xs) =
	let e1 :: rev (rev (x:xs)) === x : rev (rev xs)
	    e1 = revAppendP (rev xs) (x:Nil)

	    e2 :: x : rev (rev xs) === x : xs
	    e2 = subst (cons x)	(revRevP xs)
	in  transH e1 e2

{-

appendNilP :: forall * . xs ++ Nil ==== xs
appendNilP (Nil) =
	Nil ++ Nil			={ DEF }
	Nil
appendNilP (x : xs) =
	(x:xs) ++ Nil			={ DEF }
	x:(xs ++ Nil)			={ appendNilP xs }
	x:xs

appendAssocP :: forall * . (xs++ys)++zs ==== xs++(ys++zs)
appendAssocP (Nil) ys zs =
	(Nil ++ ys) ++ zs		={ DEF }
	ys ++ zs
appendAssocP (x:xs) ys zs =
	((x : xs) ++ ys) ++ zs		={ DEF }
	(x : (xs ++ ys)) ++ zs		={ DEF }
	x : ((xs ++ ys) ++ zs)		={ appendAssocP zs ys zs }
	x : (xs ++ (ys ++ zs))		={ DEF }
	(x : xs) ++ (ys ++ zs)

revAppendP :: forall * . rev (xs ++ ys) ==== rev ys ++ rev xs
revAppendP (Nil) ys =
	rev (Nil ++ ys)			={ DEF }
	rev ys				={ symm (appendNilP (rev ys)) }
	rev ys ++ Nil			={ DEF }
	rev ys ++ rev Nil
revAppendP |a (x:xs) ys =
	rev ((x : xs) ++ ys)		={ DEF }
	rev (x : (xs ++ ys))		={ DEF }
	rev (xs ++ ys) ++ (x:Nil)	={ revAppendP xs ys }
	(rev ys ++ rev xs) ++ (x:Nil)	={ appendAssocP (rev ys) (rev xs) (x:Nil) }
	rev ys ++ (rev xs ++ (x:Nil))	={ DEF }
	rev ys ++ (rev (x:xs))

revRevP :: forall * . rev (rev xs) ==== xs
revRevP (Nil) =
	rev (rev Nil)			={ DEF }
	rev Nil				={ DEF }
	Nil
revRevP (x:xs) =
	rev (rev (x:xs))		={ DEF }
	rev (rev xs ++ (x:Nil))		={ revAppendP (rev xs) (x:Nil) }
	rev (x:Nil) ++ rev (rev xs)	={ revRevP xs }
	rev (x:Nil) ++ xs		={ DEF }
	(x:Nil) ++ xs			={ DEF }
	x:xs

------

eqs ::=	exp
	eqs ={ DEF } exp
	eqs ={ prf } exp 
l(exp) = exp
l(eqs ={ _ } exp) = exp
h(exp) = exp
h(eqs ={ _ } exp) = h(eqs)

	E[e]               =  refl e ::      e ==== e
	E[eqs ={ DEF } e]  =  E[eqs] :: h(eqs) ==== e
	E[eqs ={ p } C[e]] =  trans h(eqs) l(eqs) e
				    (E[eqs])
				    (subst l(eqs) e (\ (x::T) -> C[x]) p :: C[l(eqs)] ==== C[e])
				     :: h(eqs) ==== e
				IF   e::T
-}
