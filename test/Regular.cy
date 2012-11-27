module example$Regular =
#include Prelude
open example$Bifunctor use * in
open example$Tree use Tree, Leaf, Bin in

struct

data Regular = ListType | TreeType --| UserType String

concrete
FunctorOf :: Regular -> # -> # -> #
FunctorOf d = Fmap (functorOf d)

concrete
Map :: Regular -> # -> #
Map (ListType) = List 
Map (TreeType) = Tree 

functorOf :: Regular -> Bifunctor
functorOf (ListType) = fList 
functorOf (TreeType) = fTree 

-- data List a = Nil | (:) a (List a)
concrete
fList :: Bifunctor = Plus Empty (Times Par Rec)

-- data Tree a = Leaf a | Bin (Tree a) (Tree a)
concrete
fTree :: Bifunctor = Plus Par   (Times Rec Rec)

inn :: (a :: #) |-> (d :: Regular) -> FunctorOf d a (Map d a) -> Map d a
inn |a (ListType)   = inn_List |a
inn |a (TreeType)   = inn_Tree |a
--inn |a (UserType u) = error "not yet" -- inn_u |a

inn_List :: (a :: #) |-> Fmap fList a (List a) -> List a
inn_List (Left (unit)) = Nil
inn_List (Right (x,l)) = x : l

inn_Tree :: (a :: #) |-> Fmap fTree a (Tree a) -> Tree a
inn_Tree (Left  x    ) = Leaf x
inn_Tree (Right (l,r)) = Bin l r

out :: (a :: #) |-> (d :: Regular) -> Map d a -> FunctorOf d a (Map d a) 
out |a (ListType)   = out_List |a
out |a (TreeType)   = out_Tree |a
--out |a (UserType u) = error "not yet" -- out_u |a

out_List :: (a :: #) |-> List a -> Fmap fList a (List a) 
out_List (Nil) = Left  unit
out_List (x:l) = Right (x,l)

out_Tree :: (a :: #) |-> Tree a -> Fmap fTree a (Tree a) 
out_Tree (Leaf x ) = Left  x    
out_Tree (Bin l r) = Right (l,r)
