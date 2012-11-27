module UIdSet(UIdSet, union, intersect, minus, elem, singleton, delete, unions, empty, fromList, null, showUIdSet) where
import Prelude hiding(elem, null)
import Id
import qualified Data.IntSet as I

newtype UIdSet = U I.IntSet

union :: UIdSet -> UIdSet -> UIdSet
union (U s1) (U s2) = U (I.union s1 s2)

intersect :: UIdSet -> UIdSet -> UIdSet
intersect (U s1) (U s2) = U (I.intersection s1 s2)

minus :: UIdSet -> UIdSet -> UIdSet
minus (U s1) (U s2) = U (I.difference s1 s2)

elem :: UId -> UIdSet -> Bool
elem i (U s) = I.member (getUIdNo i) s

singleton :: UId -> UIdSet
singleton i = U (I.singleton (getUIdNo i))

empty :: UIdSet
empty = U I.empty

delete :: UId -> UIdSet -> UIdSet
delete i (U s) = U (I.delete (getUIdNo i) s)

unions :: [UIdSet] -> UIdSet
unions ss = foldr union empty ss

fromList :: [UId] -> UIdSet
fromList is = U (I.fromList (map getUIdNo is))

null :: UIdSet -> Bool
null (U s) = I.null s

showUIdSet :: UIdSet -> String
showUIdSet (U s) = show (I.toList s)
