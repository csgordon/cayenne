module System$String = struct
-- To avoid cyclic dependencies we need local versions of the List type
private type List a = data Nil | (:) a (List a)

type String = List System$CharType.Char
