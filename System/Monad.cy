module concrete System$Monad = \ (m :: # -> #) -> sig
(>>=) :: (a :: #) |-> (b :: #) |-> m a -> (a -> m b) -> m b
(>>) :: (a :: #) |-> (b :: #) |-> m a -> m b -> m b
return :: (a :: #) |-> a -> m a
