module System$Id =
-- Good ole type theory Id type.
-- By Magnus Carlsson.
struct

(===) :: (a :: #) |-> a -> a -> #
(===) x y = data refl

refl :: (a :: #) |-> (x :: a) |-> x === x
refl |a |x = refl@(x === x)

private
reflE :: (a :: #) |-> (x :: a) -> x === x
reflE x = refl@(x === x)

--concrete
native idPeel :: (a :: #) |-> 
	(C :: (x, y :: a) -> x === y -> #) ->
	(d :: (x :: a) -> C x x (reflE x)) ->
	(x, y :: a) ->
	(p :: x === y) ->
	C x y p
    = "\\d -> \\x -> \\y -> \\p -> d x"

-- Some simple and useful theorems.
symm :: (a :: #) |-> (x, y :: a) |-> 
	x === y -> y === x
symm |a |x |y = idPeel (\ (x, y :: a) (p :: x === y) -> y === x)
             	       (\ (x :: a) -> reflE x) x y

private
trans' :: (a :: #) |-> 
          (x, y, z :: a) -> y === z -> x === y -> x === z
trans' |a x = idPeel (\ (y, z :: a) (p :: y === z) -> x === y -> x === z)
                     (\ (y :: a) (p :: x === y) -> p)

trans :: (a :: #) |-> (x, y, z :: a) |->
	x === y -> y === z -> x === z
trans |a |x |y |z p q = trans' x y z q p

congr :: (a :: #) |-> 
	(P :: a -> #) -> (x, y :: a) -> x === y -> P x -> P y
congr |a P = idPeel (\ (x, y :: a) (p :: x === y) -> P x -> P y)
                    (\ (d :: a) (x :: P d) -> x)

-- Substitutivity
subst :: (a, b :: #) |->  (x, y :: a) |-> (f :: a -> b) ->
	 x === y -> f x === f y
subst |a |b |x |y f p = congr (\ (z :: a) -> f x === f z) x y p (reflE (f x))

-- Extensionality axiom
native ext :: (a, b :: #) |-> (f, g :: a -> b) |-> 
	((x :: a) -> f x === g x) -> f === g
    = "\\f -> \\g -> \\p -> p False"
