native "import IO";
native "import System";
module System$IO = 
open System$String use String in
open System$Unit use Unit in
struct

abstract data IO a = IO_XXX a

abstract data IOError = IOError_XXX

type FilePath = String

native readFile :: FilePath -> IO String = "readFile"

native writeFile :: FilePath -> String -> IO Unit = "writeFile"

native exitWith :: System$Int.Int -> IO Unit = "exitWith"

native catch :: (a :: #) |-> IO a -> (IOError -> IO a) -> IO a = "catch"

native getArgs :: IO (System$List.List String) = "getArgs"

native userError :: String -> IOError = "userError"

native ioError :: (a :: #) |-> IOError -> IO a = "ioError"

abstract data Handle = Handle_XXX

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

native stdin :: Handle = "stdin"
native stdout :: Handle = "stdout"
native stderr :: Handle = "stderr"

native openFile :: FilePath -> IOMode -> IO Handle = "openFile"

native hPutStr :: Handle -> String -> IO Unit = "hPutStr"

native hPutChar :: Handle -> System$Char.Char -> IO Unit = "hPutChar"

native hGetChar :: Handle -> IO System$Char.Char = "hGetChar"

native hGetContents :: Handle -> IO String = "hGetContents"

native hClose :: Handle -> IO Unit = "hClose"

Monad_IO :: System$Monad IO
Monad_IO = struct
    native return :: (a :: #) |-> a -> IO a = "(return::(a->IO a))"
    native (>>=) :: (a :: #) |-> (b :: #) |-> IO a -> (a -> IO b) -> IO b = "((>>=)::(IO a->(a->IO b)->IO b))"
    (>>) :: (a :: #) |-> (b :: #) |-> IO a -> IO b -> IO b
    (>>) |a |b ioa iob = ioa >>= \ (_ :: a) -> iob


putStr :: String -> IO Unit = hPutStr stdout
putStrLn :: String -> IO Unit = \ (s::String) -> Monad_IO.(>>) (hPutStr stdout s) (hPutStr stdout "\n")

interact :: (String -> String) -> IO Unit
interact f = do Monad_IO
    (inp :: String) <- hGetContents stdin
    putStr (f inp)

private
hGetLine :: Handle -> IO String
hGetLine h = do Monad_IO
        (c :: System$Char.Char) <- hGetChar h 
	System$Bool.if (System$Char.(==) c '\n') (return "") (do Monad_IO {
	    (cs :: String) <- hGetLine h;
	    return (System$List.(:) c cs);
            })

getLine :: IO String
getLine = hGetLine stdin
