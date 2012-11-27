-- Copyright (c) 1982-1999 Lennart Augustsson, Thomas Johnsson
-- See LICENSE for the full license.
--
module ListUtil(lookupWithDefault, mapFst, mapSnd, chopList) where

-- Repeatedly extract (and transform) values until a predicate hold.  Return the list of values.
unfoldr :: (a -> (b, a)) -> (a -> Bool) -> a -> [b]
unfoldr f p x | p x       = []
	      | otherwise = y:unfoldr f p x'
			      where (y, x') = f x

chopList :: ([a] -> (b, [a])) -> [a] -> [b]
chopList f l = unfoldr f null l

mapFst f xys = [(f x, y) | (x, y) <- xys]
mapSnd f xys = [(x, f y) | (x, y) <- xys]

lookupWithDefault :: (Eq a) => [(a, b)] -> b -> a -> b
lookupWithDefault [] d _ = d
lookupWithDefault ((x,y):xys) d x' = if x == x' then y else lookupWithDefault xys d x'
