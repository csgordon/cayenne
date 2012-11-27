module example$testBase =
open example$Regular use Regular, FunctorOf, functorOf, Map, inn, out in
open example$PMap    use fmap in
open example$Base    use cata, ana in
open System$HO       use id, (·) in

struct

----------------------------------------------------------------
-- A highly useless polytypic identity function using cata
pid :: (a :: #) |-> (d :: Regular) -> Map d a -> Map d a
pid |a d = cata |a |(Map d a) d (inn |a d)

----------------------------------------------------------------
-- A highly useless polytypic identity function using ana
pida :: (a :: #) |-> (d :: Regular) -> Map d a -> Map d a
pida |a d = ana |a |(Map d a) d (out |a d)

----------------------------------------------------------------
-- Reimplementing pmap using cata
pmapc :: (a,b :: #) |-> (d :: Regular) -> (a->b) -> Map d a -> Map d b
pmapc |a |b d g = 
  let fun :: FunctorOf d a (Map d b) -> Map d b
      fun x = inn |b d (fmap (functorOf d) g (id|(Map d b)) x)
  in cata |a |(Map d b) d fun

----------------------------------------------------------------
-- Reimplementing pmap using ana
pmapa :: (a,b :: #) |-> (d :: Regular) -> (a->b) -> Map d a -> Map d b
pmapa |a |b d g = 
  let fun :: Map d a -> FunctorOf d b (Map d a)
      fun x = fmap (functorOf d) g (id|(Map d a)) (out |a d x)
  in ana |b |(Map d a) d fun
