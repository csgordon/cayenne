module example$set =
#include Prelude
struct

type IntSet = sig
    type T
    empty :: T
    single :: Int->T
    union :: T->T->T
    member :: Int->T->Bool

naïveSet :: IntSet
naïveSet = struct
    abstract type T = Int->Bool
    empty x = False
    single x x' = x == x'
    union s t x = s x || t x
    member x s = s x

cleverSet :: IntSet
cleverSet = struct
    abstract data T = Nil | Leaf Int | Fork T T
    empty = Nil
    single x = Leaf x
    union :: T->T->T
    union (Nil) s = s
    union (Leaf x) s = insert x s
    union (Fork s t) (Fork s' t') = Fork (union s s') (union t t')
    union s (Nil) = s
    union s (Leaf x) = insert x s
    member :: Int->T->Bool
    member x (Nil) = False
    member x (Leaf x') = x == x'
    member x (Fork l r) = 
	if (odd x) 
	   (member (x `quot` 2) l)
	   (member (x `quot` 2) r)
    private
    insert :: Int -> T -> T
    insert x (Nil) = Leaf x
    insert x s@(Leaf x') =
	if (x == x')
	   s
	   (insert x (insert x' (Fork Nil Nil)))
    insert x (Fork l r) =
	if (odd x)
	   (Fork (insert (x `quot` 2) l) r)
	   (Fork l (insert (x `quot` 2) r))
