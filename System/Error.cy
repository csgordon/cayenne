module System$Error =
struct
native error :: (a :: #) |-> System$String.String -> a = "\\s -> error s"

native Error :: System$String.String -> # = "\\s -> error s"

native ErrorT :: System$String.String -> #1 = "\\s -> error s"

undefined | (a :: #) :: a = error "undefined"

Undefined :: # = Error "Undefined"

UndefinedT :: #1 = ErrorT "UndefinedT"
