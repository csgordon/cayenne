module foo$dep = 
open System$Bool use Bool, True, False in
open System$Int use Int, show in
open System$String use String in
struct

FT :: Bool -> #
FT (True) = Int
FT (False) = String

f :: (x::Bool) -> FT x
f (True) = 1
f (False) = "a"

{-
st (x::Bool) :: # =
    case x of { 
    True -> Int->String; 
    False -> String->String;
    };
s (x::Bool) :: st x =
    case x of {
    True -> show;
    False -> \(s::String)->s;
    } :: (\ (x::Bool) -> st x);
ss (x::Bool) :: String = --(s x) (f x);
    case x of {
    True -> (s x) (f x);
    False -> (s x) (f x);
    };
-}
