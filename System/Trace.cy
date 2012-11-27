--@ The System$Trace module contains a side effecting function that prints
--@ its first argument and returns the second.  It is intended for debugging.
native "import Debug.Trace(trace)";
module System$Trace = struct
native trace :: (a :: #) |-> System$String.String -> a -> a = "\\s -> \\x -> trace s x"
