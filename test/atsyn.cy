module example$atsyn =
struct
open System$List use List, Nil, (:), elem, foldr, map
open System$Bool use Bool, if
open System$Integer use Integer, (+)
open System$HO use id

{-
class Eq e where
    (==) :: e -> e -> Bool
-}
classEq :: # -> #
classEq e = sig
    (==) :: e -> e -> Bool

instanceEqInteger :: classEq Integer
instanceEqInteger = struct
    (==) = System$Integer.(==)

{-
class Collects c where
    type Elem c
    empty :: c
    insert :: Elem c -> c -> c
    toList :: c -> [Elem c]
-}

classCollects :: # -> #1.0
classCollects c = sig
    type Elem {-c-}
    empty :: c
    insert :: Elem {-c-} -> c -> c
    toList :: c -> List (Elem {-c-})

{-
instance (Eq e) => Collects [e] where
    type Elem [e] = e
    empty = []
    insert x xs = if x `elem` xs then xs else x : xs
    toList xs = xs
-}

instanceCollectsList :: (e :: #) -> classEq e -> classCollects (List e)
instanceCollectsList e eqD = struct
    abstract type Elem {-c-} = e
    empty = Nil
    insert x xs = if (elem (eqD.(==)) x xs) xs (x : xs)
    toList xs = xs

-- I think this is not the best way to do type equality, but it will do.
EqType :: # -> # -> #
EqType s t = sig
    conv :: s -> t
    vnoc :: t -> s

reflEqType :: (s :: #) -> EqType s s
reflEqType s = struct { conv s = s; vnoc t = t }

----------------

sum :: List Integer -> Integer
sum = foldr (+) 0

sumColl :: (c :: #) -> (collectsD :: classCollects c) -> 
	EqType (collectsD.Elem {-c-}) Integer -> c -> Integer
sumColl c collectsD eqT co = sum (map eqT.conv (collectsD.toList co))

{-
co :: [Integer]
co = insert 1 (insert 1 (insert 2 empty))
-}

co :: List Integer
co =
    let collD = instanceCollectsList Integer instanceEqInteger
    in  collD.insert 1 (collD.insert 2 collD.empty)

{-
sco :: Integer
sco = sumColl co
-}

sco :: Integer
sco =
    let collD = instanceCollectsList Integer instanceEqInteger
	eqD = reflEqType Integer
    in  sumColl (List Integer) collD eqD co
