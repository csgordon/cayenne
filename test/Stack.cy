module concrete example$STACK = sig
type Stack a
empty :: (a :: #) |-> Stack a
push :: (a :: #) |-> a -> Stack a -> Stack a
pop :: (a :: #) |-> Stack a -> Stack a
top :: (a :: #) |-> Stack a -> a
isEmpty :: (a :: #) |-> Stack a -> System$Bool.Bool

