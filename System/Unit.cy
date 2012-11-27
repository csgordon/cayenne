module System$Unit =
open System$String use String in
struct
data Unit = unit

show :: Unit -> String
show (unit) = "unit"

