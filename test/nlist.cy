module example$nlist =
--#include Prelude
open System$Bool use * in
open System$Int use * in
open System$Unit use * in
struct
private 
ifT :: Bool -> # -> # -> #
ifT (False) t f = f
ifT (True)  t f = t

abstract
type List a = sig
    null :: Bool 
    head :: ifT null Unit a
    tail :: ifT null Unit (List a)

cons :: (a :: #) |-> a -> List a -> List a
cons x xs = struct { null = False; head = x; tail = xs }

nil :: (a :: #) |-> List a
nil |a = struct { null = True; head = unit; tail = unit }

length :: (a :: #) |-> List a -> Int
length |a l = length' |a l.null l.tail

private
length' :: (a :: #) |-> (n::Bool) -> ifT n Unit (List a) -> Int
length' |a (True)  t = 0
length' |a (False) t = 1 + length |a t

hd :: (a :: #) |-> (l :: List a) -> ifT l.null Unit a
hd |a l = l.head

tl :: (a :: #) |-> (l :: List a) -> ifT l.null Unit (List a)
tl |a l = l.tail
