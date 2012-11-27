module example$Base =
#include Prelude
open example$Regular use * in
open example$PMap    use fmap in
open System$HO       use * in

struct

----------------------------------------------------------------
-- catamorphism = generalised fold
cata :: (a,b :: #) |-> (d :: Regular) -> 
        (FunctorOf d a b -> b) -> 
         Map d a -> b
cata |a |b d i = 
   i · fmap (functorOf d) (id|a) (cata |a |b d i) · out |a d

----------------------------------------------------------------
-- anamorphism = generalised unfold
ana :: (a,b :: #) |-> (d :: Regular) -> 
       (b -> FunctorOf d a b) -> 
        b -> Map d a
ana |a |b d o = 
   inn |a d · fmap (functorOf d) (id|a) (ana |a |b d o) · o
