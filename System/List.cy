module System$List = 
open System$String use String in
open System$Bool use Bool, True, False, if, (&&), (||) in
open System$Int use Int, (+), (==), (-) in
struct
data List a = Nil | (:) a (List a)

null :: (a :: #) |-> List a -> Bool
null (Nil) = True
null (_ : _) = False

length :: (a :: #) |-> List a -> Int
length (Nil) = 0
length (_ : xs) = 1 + length xs

map :: (a :: #) |-> (b :: #) |-> (a->b) -> List a -> List b
map f (Nil) = Nil
map f (x : xs) = f x : map f xs

filter :: (a :: #) |-> (a->Bool) -> List a -> List a
filter p (Nil) = Nil
filter p (x : xs) = if (p x) (x : (filter p xs)) (filter p xs)

elem :: (a :: #) |-> (a->a->Bool) -> a -> List a -> Bool
elem eq _ (Nil)    = False
elem eq x (y:ys)   = x `eq` y || elem eq x ys

foldr :: (a, b :: #) |-> (a -> b -> b) -> b -> List a -> b
foldr f z (Nil) = z
foldr f z (x : xs) = f x (foldr f z xs)

concat :: (a :: #) |-> List (List a) -> List a
concat (Nil) = Nil
concat (xs : xss) = xs ++ concat xss

reverse :: (a :: #) |-> List a -> List a
reverse l       =  
   let rev :: List a -> List a -> List a
       rev (Nil)  a = a
       rev (x:xs) a = rev xs (x:a)
   in rev l Nil

intersperse :: (a :: #) |-> a -> List a -> List a
intersperse sep (Nil)     = Nil
intersperse sep (x:(Nil)) = x:Nil
intersperse sep (x:xs)    = x : sep : intersperse sep xs

head ::  (a :: #) |-> List a -> a
head (Nil) = System$Error.error "System$List.head Nil"
head (x : _) = x

tail ::  (a :: #) |-> List a -> List a
tail (Nil) = System$Error.error "System$List.tail Nil"
tail (_ : xs) = xs

show :: (a :: #) |-> (a->String) -> List a -> String
show s (Nil) = "Nil"
show s (x : xs) = s x ++ " : " ++ show s xs

(++) :: (a :: #) |-> List a -> List a -> List a
(++) (Nil) ys = ys
(++) (x : xs) ys = x : (xs ++ ys)

take :: (a :: #) |-> Int -> List a -> List a
take |a n (Nil) = Nil
take |a n (x : xs) = if (n == 0) (Nil|a) (x : take (n-1) xs)

drop :: (a :: #) |-> Int -> List a -> List a
drop n (Nil) = Nil
drop n (x : xs) = if (n == 0) xs (drop (n-1) xs)

zipWith :: (a :: #) |-> (b :: #) |-> (c :: #) |-> 
        (a -> b -> c) -> List a -> List b -> List c
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys
zipWith f _        _        = Nil

private data Pair a b = (,) a b
zip :: (a, b :: #) |->
       List a -> List b -> List (Pair a b)
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _        _        = Nil

Monad_List :: System$Monad List = struct
    (>>=) :: (a :: #) |-> (b :: #) |-> List a -> (a -> List b) -> List b
    (>>=) |a |b xs f = concat (map|a|(List b) f xs)
    (>>) :: (a :: #) |-> (b :: #) |-> List a -> List b -> List b
    (>>) |a |b x y = x >>= (\ (_::a) -> y)
    return :: (a :: #) |-> a -> List a
    return x = x : Nil

equal :: (a :: #) |-> (a -> a -> Bool) -> List a -> List a -> Bool
equal eq (Nil) (Nil) = True
equal eq (x : xs) (y : ys) = eq x y && equal eq xs ys
equal eq _ _ = False
