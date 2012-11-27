native "#include \"ref.t\"";

module System$ST =

open System$Tuples use Pair, (,) in
open System$Bool use Bool, if in
open System$Unit use Unit, unit in
open System$HO use uncurry in
open System$Int use Int, (==) in

struct

-- State monad, Moggi & Sabry style

concrete
ST :: #1.0
ST = sig
    type Var _
    type M _
    m        :: System$Monad M
    newVar   :: (A :: #) |-> A -> M (Var A)
    readVar  :: (A :: #) |-> Var A -> M A
    writeVar :: (A :: #) |-> Var A -> A -> M Unit
    sameVar  :: (A :: #) |-> (B :: #) |-> Var A -> Var B -> Bool

runST :: (A :: #) |-> ((s :: ST) -> s.M A) -> A
runST f = 

   let data State = state
       abstract native Ref :: # -> # = "SUM"
       native Pref :: (A :: #) |-> A -> Ref A = "Pref"
       native Passign :: (A :: #) |-> Ref A -> A -> Unit = "Passign"
       native Pderef :: (A :: #) |-> Ref A -> A = "Pderef"
       native Peqref :: (A :: #) |-> (B :: #) |-> Ref A -> Ref B -> Int
                     = "Peqref"

       data Lift A = lift A

       st :: ST
       st = struct 
           abstract 
           type Var A = Ref (Lift A)

           abstract type M A = State -> Pair A State

           m = struct
                 return x s = (x,s)
                 (>>=) c f s = uncurry f (c s)
                 (>>) |A |B a b = a >>= (\ (_ :: A) -> b)

           newVar x     (state) = case Pref (lift x) of unit -> (unit,state)
           readVar r    (state) = case Pderef r of (lift v)  -> (v,state)
           writeVar r x (state) = case Passign r (lift x) of 
                                                      (unit) -> (unit,state)
           sameVar r1 r2 = Peqref r1 r2 == 1

   in case f st state of (r, _) -> r
