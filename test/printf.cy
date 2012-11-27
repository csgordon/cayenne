module example$printf =
#include Prelude
struct
concrete
PrintfType :: String -> #
PrintfType (Nil)        = String
PrintfType ('%':'d':cs) = Integer-> PrintfType cs
PrintfType ('%':'s':cs) = String -> PrintfType cs
PrintfType ('%': _ :cs) =           PrintfType cs
PrintfType ( _ :cs)     =           PrintfType cs

private 
printf' :: (fmt::String) -> String -> PrintfType fmt
printf' (Nil)        out = out
printf' ('%':'d':cs) out = \ i -> printf' cs (out ++ show i)
printf' ('%':'s':cs) out = \ s -> printf' cs (out ++ s)
printf' ('%': c :cs) out =        printf' cs (out ++ c : Nil)
printf' (c:cs)       out =        printf' cs (out ++ c : Nil)

printf :: (fmt::String) -> PrintfType fmt
printf fmt = printf' fmt Nil
