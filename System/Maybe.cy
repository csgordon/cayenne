module System$Maybe = 
open System$List use (++) in
struct
data Maybe a = Nothing | Just a

maybe :: (a :: #) |-> (b :: #) |-> b -> (a -> b) -> Maybe a -> b
maybe n j (Nothing) = n
maybe n j (Just x) = j x

fromJust :: (a :: #) |-> Maybe a -> a
fromJust (Nothing) = System$Error.error "System$Maybe.fromJust: Nothing"
fromJust (Just x)  = x

Monad_Maybe :: System$Monad Maybe = struct
    (>>=) :: (a :: #) |-> (b :: #) |-> Maybe a -> (a -> Maybe b) -> Maybe b
    (>>=) (Nothing) f = Nothing
    (>>=) (Just x) f = f x
    (>>) :: (a :: #) |-> (b :: #) |-> Maybe a -> Maybe b -> Maybe b
    (>>) |a |b x y = x >>= (\ (x::a) -> y)
    return :: (a :: #) |-> a -> Maybe a
    return x = Just x

show :: (a :: #) |-> (a -> System$String.String) -> Maybe a -> System$String.String
show s (Nothing) = "Nothing"
show s (Just x) = "(Just " ++ s x ++ ")"
