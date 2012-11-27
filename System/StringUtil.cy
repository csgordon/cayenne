module System$StringUtil =
open System$String use String in
struct
(==) :: String -> String -> System$Bool.Bool
(==) s t = System$List.equal System$Char.(==) s t
