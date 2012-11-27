module concrete example$QUEUE = {
Queue :: # -> #;
empty :: (a :: #) |-> Queue a;
enqueue :: (a :: #) |-> a -> Queue a -> Queue a;
dequeue :: (a :: #) |-> Queue a -> Queue a;
first :: (a :: #) |-> Queue a -> a;
};
