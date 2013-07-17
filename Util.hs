module Util(mkSet, unions, assoc, sortFst, findDup, allSame, sortGroup, boolCompress,
       revTake, revDrop, breakAt,
       tracex, traces, doFull, tracex2, doTrace, doTrace2
       ) where
import Data.Function(on)
import Data.List(sort, sortBy, group, groupBy, union, partition)

import Libs.Trace
import Libs.IOUtil (progArgs)

doTrace = elem "-debug" progArgs
doTrace2 = length (filter (== "-debug") progArgs) > 1
doFull = elem "-full" progArgs

tracex s x = if doTrace then traces s x else x
tracex2 s x = if doTrace2 then traces s x else x

traces s x = if s==s then trace s x else undefined


remDup :: (Eq a) => [a] -> [a]
remDup (x:xxs@(x':_)) = if x==x' then remDup xxs else x:remDup xxs
remDup xs = xs

breakAt x xs =
	case span (/= x) xs of
	    (ys,_:zs) -> (ys,zs)
	    p -> p

unions l = foldr union [] l

revTake n = reverse . take n . reverse
revDrop n = reverse . drop n . reverse

boolCompress [] _  = []
boolCompress _  [] = []
boolCompress (True:bs) (x:xs) = x : boolCompress bs xs
boolCompress (False:bs) (x:xs) = boolCompress bs xs

anySame :: (Ord a) => [a] -> Bool
anySame = same . sort
	where same (x:xs@(x':_)) = x == x' || same xs
	      same _ = False

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (==x) xs

mkSet :: (Ord a) => [a] -> [a]
mkSet l = remDup (sort l)

findDup [] = []
findDup (x:xs) =
    case filter (== x) xs of
    [] -> findDup xs
    xs' -> x:xs'

assoc :: (Eq a) => [(a,b)] -> a -> b
assoc xys x =
    case lookup x xys of
    Just y -> y
    Nothing -> error "assoc"

sortFst xs = sortBy (compare `on` fst) xs

sortGroup :: (a->a->Ordering) -> [a] -> [[a]]
sortGroup cmp = groupBy (\x y-> cmp x y == EQ) . sortBy cmp
