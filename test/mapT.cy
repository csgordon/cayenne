module example$mapT = 
open System$List use List, Nil, (:), map, zip in
open System$Tuples use Pair, (,) in
open System$HO use (·) in
struct

-- Non-empty list of types.
concrete
ListT :: #1.0
ListT = data OneT # | ConsT # ListT
concrete
OneT = OneT@ListT
concrete
ConsT = ConsT@ListT

concrete
LListT :: ListT -> ListT
LListT (OneT t) = OneT (List t)
LListT (ConsT t ts) = ConsT (List t) (LListT ts)

concrete
FunT :: ListT -> # -> #
FunT (OneT t) a = t -> a
FunT (ConsT t ts) a = t -> FunT ts a

private
ProdT :: ListT -> #
ProdT (OneT t) = t
ProdT (ConsT t ts) = Pair t (ProdT ts)

private
curryT :: (ts :: ListT) -> (a :: #) -> (ProdT ts -> a) -> FunT ts a
curryT (OneT t) a f = \ (x :: t) -> f x
curryT tts@(ConsT t ts) a f = \ (x :: t) -> curryT ts a (\ (xs :: ProdT ts) -> f (x,xs))

private
uncurryT :: (ts :: ListT) -> (a :: #) -> FunT ts a -> (ProdT ts -> a)
uncurryT (OneT t) a f = \ (x :: t) -> f x
uncurryT tts@(ConsT t ts) a f = \ (p :: ProdT tts) -> case p of (x,xs) -> uncurryT ts a (f x) xs

private
zipT :: (ts :: ListT) -> ProdT (LListT ts) -> List (ProdT ts)
zipT (OneT t) xs = xs
zipT (ConsT t ts) (xs, pxs) = zip xs (zipT ts pxs)
	
-- Use the following identity: map2 f = curry (map (uncurry f) . zip)

mapT :: (ts :: ListT) -> (a :: #) |-> FunT ts a -> FunT (LListT ts) (List a)
mapT ts|a f = curryT (LListT ts) (List a) (map (uncurryT ts a f) · zipT ts)

