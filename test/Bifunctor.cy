module example$Bifunctor =
open System$Unit   use Unit   in
open System$Tuples use Pair   in
open System$Either use Either in

struct

data Bifunctor = Plus  Bifunctor Bifunctor 
               | Times Bifunctor Bifunctor
               | Empty 
               | Par   -- select first argument
               | Rec   -- select second argument

-- syntactic objects of type Bifunctor corresponds to 
-- typefunctions of type # -> # -> #

concrete
Fmap :: (f :: Bifunctor) -> # -> # -> #
Fmap (Empty)     p r = Unit
Fmap (Par)       p r = p
Fmap (Rec)       p r = r
Fmap (Plus  g h) p r = Either (Fmap g p r) (Fmap h p r)
Fmap (Times g h) p r = Pair   (Fmap g p r) (Fmap h p r)

