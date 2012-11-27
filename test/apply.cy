module example$apply =
#include Prelude

struct
concrete
ApplyType :: (a :: #) -> (b :: #) -> (List a) -> #
ApplyType a b (Nil) = b
ApplyType a b (_ : ps) = a -> ApplyType a b ps

private
apply' :: (a :: #) -> (b :: #) -> (l :: List a) -> ApplyType a b l -> b
apply' a b (Nil) f = f
apply' a b (p:ps) f = apply' a b ps (f p)

apply :: (a :: #) |-> (b :: #) |-> (l :: List a) -> ApplyType a b l -> b
apply |a |b ps f = apply' a b ps f
