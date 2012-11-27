module example$StackToQueue :: example$STACK -> example$QUEUE =
\ (s :: example$STACK) ->
#include Prelude
open s use Stack, push, pop, top, isEmpty in
{
abstract type Queue a = Stack a;

empty :: (a :: #) |-> Queue a;
empty |a = s.empty|a;

enqueue :: (a :: #) |-> a -> Queue a -> Queue a;
enqueue x xs = app xs x;

dequeue :: (a :: #) |-> Queue a -> Queue a;
dequeue xs = pop xs;

first :: (a :: #) |-> Queue a -> a;
first xs = top xs;

private 
app :: (a :: #) |-> Stack a -> a -> Stack a;
app |a xs y =
    if (isEmpty xs) 
       (push y (s.empty|a))
       (push (top xs) (app (pop xs) y));
};

