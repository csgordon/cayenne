module -- lml_IO
#define File _FILE
#include "dialog.t"
export 
    m_hPutChar, m_hGetChar,
    m_hPutStr, m_readFile, m_writeFile, m_exitWith, m_catch, 
    m_getArgs, m_userError, m_hGetContents, m_openFile, m_hClose,
    m_return, m_bind, m_fail, ioToDialog,
    m_stdin, m_stdout, m_stderr;
rec
    type IOT *a == (*a -> Dialog) -> Dialog
and type IO *a == IOT (OK CY_IOError *a)
and type CY_IOError = IOError IOErrorCode (Option Handle) (Option String)
and type IOErrorCode = EOF + User String + PosixErrno Int String
and type Unit = unit
and type Handle = Handle IOMode File
and type IOMode = ReadMode + WriteMode + AppendMode + ReadWriteMode

and m_stdin  = Handle ReadMode _fileStdin
and m_stdout = Handle WriteMode _fileStdout
and m_stderr = Handle WriteMode _fileStderr

and return x k = k (Yes x)
and bind m f k = 
        m (\ ex .
	    case ex in
	       No e : k (No e)
	    || Yes x : (f x) k
            end)

and processRequestIO :: Request -> IO Response
and processRequestIO req = \ k . \ rs . req . k (Yes (hd rs)) (tl rs)

and processRequestIOUnit :: Option Handle -> Option String -> Request -> IO Unit
and processRequestIOUnit mh ms req =
    bind (processRequestIO req) ( \ resp .
    case resp in
       Success       : return unit
    || Failure ioerr : getIOErrorCode mh ms ioerr
    || _             : procError "Success" 
    end)

and processRequestIOFile :: Option Handle -> Option String -> Request -> IO File
and processRequestIOFile mh ms req =
    bind (processRequestIO req) ( \ resp .
    case resp in
       Fil f         : return f
    || Failure ioerr : getIOErrorCode mh ms ioerr
    || _             : procError "Fil" 
    end)

and processRequestIOString :: Option Handle -> Option String -> Request -> IO String
and processRequestIOString mh ms req =
    bind (processRequestIO req) ( \ resp .
    case resp in
       Str s         : return s
    || Failure ioerr : getIOErrorCode mh ms ioerr
    || _             : procError "Str" 
    end)

and processRequestIOInt :: Option Handle -> Option String -> Request -> IO Int
and processRequestIOInt mh ms req =
    bind (processRequestIO req) ( \ resp .
    case resp in
       IntResp s     : return s
    || Failure ioerr : getIOErrorCode mh ms ioerr
    || _             : procError "IntResp" 
    end)

and processRequestIOStringList :: Option Handle -> Option String -> Request -> IO (List String)
and processRequestIOStringList mh ms req =
    bind (processRequestIO req) ( \ resp .
    case resp in
       StrList ss         : return ss
    || Failure ioerr : getIOErrorCode mh ms ioerr
    || _             : procError "StrList" 
    end)

and procError :: String -> *a
and procError s = fail ("Bad Response, expected a " @ s)

and getIOErrorCode :: Option Handle -> Option String -> IOError -> IO *a
and getIOErrorCode h s (OtherError "EOF") =
    xfail (IOError EOF h s)
||  getIOErrorCode h s _ =
    bind (processRequestIO H_GetErrno) ( \ (IntResp i) .
    xfail (IOError (posixError i) h s))
    
and posixError i = PosixErrno i (strerror i)

and xfail :: CY_IOError -> IO *a
and xfail e = \ k . k (No e)

and ioToDialog :: IO *a -> Dialog
and ioToDialog m = 
    m (\ ex .
	case ex in
	   No e : fail (mkErrorMessage e)
	|| Yes _ : \ _ . []
        end)

and mkErrorMessage err = "Program error: " -- XXX ++ mkErrMsg err

and m_hPutStr :: Handle -> String -> IO Unit
and m_hPutStr (h as (Handle _ f)) s = processRequestIOUnit (Some h) None (H_PutString f s)

and m_hPutChar :: Handle -> Char -> IO Unit
and m_hPutChar (h as (Handle _ f)) c = processRequestIOUnit (Some h) None (H_PutChar f (ord c))

and m_hGetChar :: Handle -> IO Char
and m_hGetChar (h as (Handle _ f)) = bind (processRequestIOInt (Some h) None (H_GetChar f)) ( \ c . return (chr c))

and m_openFile :: String -> IOMode -> IO Handle
and m_openFile name mode = 
    bind (processRequestIOFile None (Some name) (H_OpenFile name (modeToInt mode))) (\ hdl .
    return (Handle mode hdl))

and m_hClose  :: Handle -> IO Unit
and m_hClose (h as (Handle _ f)) = processRequestIOUnit (Some h) None (H_Close f)

and modeToInt :: IOMode -> Int
and modeToInt ReadMode = 0
||  modeToInt WriteMode = 1
||  modeToInt AppendMode = 8
||  modeToInt ReadWriteMode = 2

and m_writeFile  :: String -> String -> IO Unit
and m_writeFile name str =
    bind (m_openFile name WriteMode) (\ hdl .
    bind (m_hPutStr hdl str) (\ _ .
    m_hClose hdl))

and m_exitWith      :: Int -> IO *a
and m_exitWith n = 
    bind (processRequestIOUnit None None (Exit n)) (\ _ .
    return (fail "System$IO.exitWith did not exit!"))

and m_hGetContents  :: Handle -> IO String
and m_hGetContents (h as (Handle _ f)) = 
    processRequestIOString (Some h) None (H_GetFile f)

and m_readFile :: String -> IO String
and m_readFile name = bind (m_openFile name ReadMode) m_hGetContents

and m_getArgs :: IO (List String)
and m_getArgs = processRequestIOStringList None None GetArgs

and catch :: IO *a -> (CY_IOError -> IO *a) -> IO *a 
and catch m h = \ k .
    m (\ ex .
	case ex in
	   No e : (h e) k
	|| Yes x : k ex
        end)

and m_catch i h = catch i h
and m_userError s = IOError (User s) None None
and m_bind io f = bind io f
and m_return x = return x
and m_fail e = xfail e
end
