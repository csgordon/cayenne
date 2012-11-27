module example$StackL :: example$STACK =
#include Prelude
{
abstract type Stack a = List a;

empty :: (a :: #) |-> Stack a;
empty |a = Nil |a;

push :: (a :: #) |-> a -> Stack a -> Stack a;
push x xs = x : xs;

pop :: (a :: #) |-> Stack a -> Stack a;
pop xs =
    case xs of {
    (Nil) -> error "pop";
    (_ : xs') -> xs';
    };

top :: (a :: #) |-> Stack a -> a;
top xs =
    case xs of {
    (Nil) -> error "top";
    (x : _) -> x;
    };

isEmpty :: (a :: #) |-> Stack a -> Bool;
isEmpty xs =
    case xs of {
    (Nil) -> True;
    (_ : _) -> False;
    };
};
