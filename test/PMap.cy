module example$PMap =
open example$Bifunctor use * in
open example$Regular use * in
open System$HO use id, (·) in
open System$Unit use Unit, unit in
open System$Tuples use Pair, (,) in
open System$Either use Either, Left, Right in

struct

fmap :: (p1,p2,r1,r2 :: #) |->
        (f::Bifunctor) -> 
        (p1->p2) -> (r1->r2) -> 
        Fmap f p1 r1 -> Fmap f p2 r2
fmap (Plus  g h) p r = fmap g p r -+- fmap h p r
fmap (Times g h) p r = fmap g p r -*- fmap h p r
fmap (Empty)     p r = id|Unit
fmap (Par)       p r = p
fmap (Rec)       p r = r

----------------------------------------------------------------
pmap :: (a,b :: #) |-> (d :: Regular) -> (a->b) -> Map d a -> Map d b
pmap |a |b d g = inn |b d · fmap (functorOf d) g (pmap d g) · out |a d

----------------------------------------------------------------

-- bimaps for the bifunctors Either and Pair

(-+-) :: (a1,a2,b1,b2 :: #) |-> 
         (a1->a2) -> (b1->b2) -> Either a1 b1 -> Either a2 b2
(-+-) f g (Left  x) = Left  (f x)
(-+-) f g (Right y) = Right (g y)

(-*-) :: (a1,a2,b1,b2 :: #) |-> 
         (a1->a2) -> (b1->b2) -> Pair   a1 b1 -> Pair   a2 b2
(-*-) f g (x,y) = (f x, g y)

