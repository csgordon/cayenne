module example$parselib =
open System$Either use Either, Left, Right in
open System$Int use Int, (>=), (==), (>) in
open System$String use String in
open System$Tuples use Pair, (,) in
open System$Bool use if, Bool, True, False, (&&) in
open System$List use List, Nil, (:), (++) in
open System$Maybe use Maybe, Just, Nothing in

\ (Token :: #) -> struct

type TList = List (Pair Int Token)


private
type ParseErr = Maybe (Pair Int (List String))
private
type ParseVal a = Pair ParseErr (Maybe (Pair TList a))

abstract
type Parser a = TList -> ParseVal a

success :: (a :: #) |-> a -> Parser a
success x l = (Nothing|(Pair Int (List String)), Just (l,x))

fail :: (a :: #) |-> String -> Parser a
fail e ((i,c):_) = (Just (i,e:Nil),Nothing|(Pair TList a))
fail e (Nil)     = (Just (9999999,e:Nil),Nothing|(Pair TList a))

match :: (a :: #) |-> (Token -> Maybe a) -> String -> Parser a
match p e (Nil) = expectfail e Nil
match p e icl@((i,c):l) = case p c of
                       (Just a) -> success a l
                       (Nothing) -> expectfail e icl

expectfail :: (a :: #) |-> String -> Parser a
expectfail s = fail ("expecting " ++ s)

private 
combine :: Bool -> ParseErr -> ParseErr -> ParseErr
combine isOr (Just err@(i,e)) (Just err'@(i',e')) = Just (if (isOr && i == i') (i,(e++e'))
                                                         (if (i > i') err err'))
combine isOr (Nothing) e' = e'
combine isOr e e' = e

combine' :: (a :: #) |-> Bool -> ParseErr -> ParseVal a -> ParseVal a
combine' isOr e (e',v) = (combine isOr e e',v)

(||) :: (a :: #) |-> Parser a -> Parser a -> Parser a
(||) p1 p2 l = case p1 l of
                   (err,(Nothing)) -> combine' True err (p2 l)
                   r -> r

bind :: (a, b :: #) |-> Parser a -> (a -> Parser b) -> Parser b
bind |a |b p1 p2 l = case p1 l of
                   (err, (Just (l,a))) -> combine' False err (p2 a l)
                   (err, (Nothing)) -> (err, Nothing|(Pair TList b))

Monad_Parser :: System$Monad Parser
Monad_Parser = struct
 return a = success a
 (>>=) a b = bind a b
 (>>) |a p1 p2 = p1 >>= (\ (_ :: a) -> p2)

concrete
monad :: System$MonadUtil.Monad
monad = struct
          m = Parser
          o = Monad_Parser


(*) :: (a,b :: #) |-> Parser (a -> b) -> Parser a -> Parser b
(*) |a |b p1 p2 = do Monad_Parser
                (f :: a ->b) <- p1
                (x :: a) <- p2
                return (f x)

(>>) :: (a,b :: #) |-> Parser a -> Parser b -> Parser b
(>>) |a |b p1 p2 = success (\ (x :: a) (y :: b) -> y) * p1 * p2

(<<) :: (a,b :: #) |-> Parser a -> Parser b -> Parser a
(<<) |a |b p1 p2 = success (\ (x :: a) (y :: b) -> x) * p1 * p2

many :: (a :: #) |-> Parser a -> Parser (List a)
many |a p = success ((:)@(List a)) * p * many p || success Nil@(List a)

parse :: (a :: #) |-> Parser a -> TList -> Either (Pair Int (List String)) (Pair TList a)
parse |a p l = case p l of
        ((Just err),(Nothing)) -> Left err
        ((Nothing),(Nothing)) -> System$Error.undefined|(Either (Pair Int (List String)) (Pair TList a))
        (err,(Just a))  -> Right a

