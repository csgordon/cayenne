module System$Prelude =
struct
-- Re-export a bunch of useful stuff
public concrete open System$Unit   use	Unit, unit
public concrete open System$String use	String
public concrete open System$List   use	List, (:), Nil, (++), Monad_List, concat, drop, elem, equal, filter, foldr,
					head, intersperse, length, map, null, reverse, {-show,-} tail, take,
					zip, zipWith
public concrete open System$Bool   use	Bool, (&&), if, ifT, not, (||), IF {-, show-}
public concrete open System$Char   use	Char, chr, ord, isAlpha, isAlphaNum, isDigit, isLower, isSpace, isSymbol, isUpper
public concrete open System$Error  use	Error, ErrorT, Undefined, UndefinedT, error, undefined
public concrete open System$IO     use	FilePath, IOMode, Handle, IO, IOError, Monad_IO, catch, exitWith, getArgs,
					getLine, hClose, hGetChar, hGetContents, hPutChar, hPutStr, interact,
					ioError, openFile, putStr, putStrLn, readFile, stderr, stdin, stdout, userError,
					writeFile
public concrete open System$Tuples use	Pair, fst, snd
public concrete open System$Either use	Either, either
public concrete open System$HO     use	const, curry, flip, id, uncurry, (·)
public concrete open System$Integer use	(*), (+), (-), (/=), (<), (<=), (==), (>), (>=), even, negate, odd, quot, rem,
					Integer, read, show
