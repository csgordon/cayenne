module System$Bool = struct
data Bool = False | True

concrete
if :: (a :: #) |-> Bool -> a -> a -> a
if (True) t e = t
if (False) t e = e

concrete
ifT :: Bool -> # -> # -> #
ifT (True) t e = t
ifT (False) t e = e

{-
concrete
If :: (c :: Bool -> #) -> (b :: Bool) -> c True -> c False -> c b
If c (True)  t f = t
If c (False) t f = f
-}

concrete
IF :: (tx :: #) |-> (ty :: #) |-> (b :: Bool) -> tx -> ty -> (ifT b tx ty)
IF (True)  x y = x
IF (False) x y = y

concrete
(&&) :: Bool -> Bool -> Bool
(&&) (True) y = y
(&&) (False) y = False

concrete
(||) :: Bool -> Bool -> Bool
(||) (True) y = True
(||) (False) y = y

concrete
not :: Bool -> Bool
not (True) = False
not (False) = True

show :: Bool -> System$String.String
show (True) = "True"
show (False) = "False"

