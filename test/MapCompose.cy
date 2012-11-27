module example$MapCompose =
open System$List use * in
open System$Id use (===), ext in

struct

concrete
map :: (a :: #) |-> (b :: #) |-> (a->b) -> List a -> List b
map f (Nil) = Nil
map f (x : xs) = f x : map f xs

concrete
(·) :: (a, b, c :: #) |->
       (b -> c) -> (a -> b) -> (a -> c)
(·) f g x = f (g x)

private
mapComposeElemP :: (a, b, c :: #) |-> (f :: b -> c) -> (g :: a -> b) -> (xs :: List a) ->
	(map f · map g) xs === map (f · g) xs
mapComposeElemP f g (Nil) =
	(map f · map g) Nil		={ DEF }=
	map (f · g) Nil
mapComposeElemP f g (x:xs) =
	(map f · map g) (x:xs)		={ DEF }=
	map f (map g (x:xs))		={ DEF }=
	f (g x) : map f (map g xs)	={ DEF }=
	(f · g) x : (map f · map g) xs	={ mapComposeElemP f g xs }=
	(f · g) x : map (f · g) xs	={ DEF }=
	map (f · g) (x:xs)

mapComposeP :: (a, b, c :: #) |-> (f :: b -> c) -> (g :: a -> b) ->
	map f · map g === map (f · g)
mapComposeP f g = ext (mapComposeElemP f g)
