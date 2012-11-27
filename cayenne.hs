module Main(main) where
import Data.List(sort, union, nub)
import Data.Maybe(fromJust)
import Data.Char(isSpace)
import System.Time
import System.IO(hPutStr, stderr)
{-import System(getArgs, getProgName, exitWith, ExitCode(..), system, getEnv)-}
import System.Environment
import System.Cmd
import System.Exit
import System.Directory(removeFile, createDirectory)
import ListUtil(chopList)
import GetPid
import Parse
import PPrint
import Util(revTake, revDrop, breakAt)
import SCC(scc)
import FString
import Error
import PreStrings
import Lex
import Id
import Position
import CSyntax
import CParser
import ISyntax
import ISyntaxPP
import CITranslate
import IReduce
import CUtil(cGlobModuleVars, cGlobSignVars, cSignId, cGlobIfcVars)
import IUtil(iGVars, iCollectDefRec)
import TypeCheck
import Simplify
import Env
import ToLML
import ToGHC
import Util(traces)
import Checker
import Binary
import Erase

version = "Cayenne version 2008-09-21\n"

dfltCayenneDir = "/usr/local/lib/cayenne"

main :: IO ()
main = do
    args <- getArgs
    pprog <- getProgName
    cdir <- getEnvDef "CAYENNEDIR" dfltCayenneDir
    case decodeArgs (baseName pprog) cdir args of
     Left emsg -> messageExit serror emsg
     Right (flags0, [name]) | suf `elem` [srcSuffix, agdaSuffix, hsSuffix] 
	   -> do
        let flags = if suf == agdaSuffix then flags0{agda=True}
		    else if suf == hsSuffix then flags0{haskell=True}
		    else flags0
        ifIO (verbose flags) $ putStr version
	file <- readFile name 
                `catch`
                \ _ -> messageExit serror ("Cannot open source file `"++name++"'\n")
        file' <- expandInclude flags file
	case parseSrc flags name file' of
	    (ErrProgram msg, _) -> messageExit serror (prEMsg msg++"\n")
	    (CProgram op mods, tbl) -> do
                mapM_ (compileModule op name suf flags tbl) mods
      where suf = reverse (fst (breakAt '.' (reverse name)))
     Right (flags, [name]) | hasSuf ifSuffixBin name -> do
	file <- readFile name 
                `catch`
                \ _ -> messageExit serror ("Cannot binary interface file `"++name++"'\n")
	let (_, ii) = iInterfaceFromBytes preStrTable file
	putStr (ppReadable (ifcIToC False ii))
     Right (flags, [name]) | isModuleString name -> do
        ifIO (verbose flags) $ putStr version
	doLink flags name
     Right (flags, []) | verbose flags ->
        putStr version
     _ -> messageExit serror (usage (baseName pprog))

compileModule :: Maybe String -> String -> String -> Flags -> StrTable -> CModule -> IO ()
compileModule op name suf flags tbl mod@(CModule nats _) = do
        let fvs = cGlobModuleVars mod
	ifIO (verbose flags) $ do
	    putStr ("Input:\n"++ppReadable mod++"\n")
{-
	ifs <- getInterfaces flags tbl (ifcPath flags) fvs []
	startEnv <- mkStartEnv op flags ifs
-}
	ifns <- getInterfacesI flags tbl (ifcPath flags) fvs []
	let (ifs, ifnames) = unzip ifns
	let lclIfs = nub [dirName (makeFileNameS n) | n <- ifnames, take 2 n == "./"]
	startEnv <- mkStartEnvI op flags ifs
	imod <-
	    case typeCheck (redSteps flags) startEnv mod of
	        Left msg -> messageExit serror (prEMsg msg++"\n")
	        Right mod' -> do
		    ifIO (verbose flags) $ do
		        putStr ("Typechecked:\n" ++
			        (if extraVerbose flags then ppDebug mod' else ppReadable mod')++"\n")
		    let mod'' = simplify startEnv mod'
		    ifIO (iface flags) $ do
		        ifIO (verbose flags) $
		            putStr "Generate interface\n"
		        genInterface (dstDir flags) name mod''
		    return mod''
	let IModule moddef@(mid, _) = imod
	    ierr = allChecks imod
	ifIO (ierr /= Nothing) $
	     internalError (fromJust ierr ++ ppDebug imod)
	ifIO (wNoMatch flags) $
	     case nub (iCollectDefRec getWarn moddef) of
	     [] -> return ()
	     ps -> mapM_ (putStrLn . prEMsg) ps
	let emod = eraseTypes (erase flags) startEnv imod
	ifIO (erase flags && verbose flags) $
	     putStr ("Erased:\n" ++ ppReadable emod)
        ifIO (lml flags) $ do
            pid <- getPid
            tmpdir <- getEnvDef "TMP" "/tmp"
	    let lmlp = toLML nats emod
	        dname = dstDir flags ++ "/" ++ makeFileName mid
		name_ = revDrop (length suf) name
                tmp = tmpdir ++ "/cayenne" ++ show pid
	        mname = tmp ++ name_ ++  lmlSuffix
	        tname = tmp ++ name_ ++ "t"
		oname = dname ++ "." ++ oSuffix
		inc = cayenneDir flags
		cmd = "lmlc -c -fno-type -I" ++ inc ++ " " ++ mname ++ " -o " ++ oname
            ifIO (verbose flags) $
	        putStr ("LML:\n"++lmlp++"\n")
	    writeFile mname lmlp
	    ifIO (lmlComp flags) $ do
	        makeDirPath oname
		ifIO (verbose flags) $
		    putStr ("exec: "++cmd++"\n")
	        rc <- system cmd
		ifIO (keepLml flags) $
                    system ("mv " ++ mname ++ " " ++ name_ ++  lmlSuffix)
		removeFile tname `catch` const (return ())
	        removeFile mname `catch` const (return ())
		ifIO (rc /= ExitSuccess) $
		    exitWith rc

        ifIO (ghc flags) $ do
            pid <- getPid
            tmpdir <- getEnvDef "TMP" "/tmp"
	    ghccomp <- getEnvDef "GHC" "ghc"
	    let (ghcp, _) = toGHC nats (init name_) emod
		modname = insertMod (makeFileName mid)
	        dname = dstDir flags ++ "/"
		name_ = revDrop (length suf) name
                tmp = tmpdir ++ "/cayenne" ++ show pid
	        mname = tmp ++ name_ ++  hsSuffix
	        tname = tmp ++ name_ ++ "hi"
		oname = dname ++ modname ++ "." ++ oSuffix
		hiname = dname ++ modname ++ "." ++ hiSuffix
		hsname = dname ++ modname ++ "." ++ hsSuffix
		inc = cayenneDir flags ++ "/System" ++ ":" ++ cayenneDir flags
		incs = inc ++ concatMap (':' :) lclIfs
		cmd = ghccomp ++ " -fglasgow-exts -c -i" ++ incs ++ " " ++ mname ++ " -o " ++ oname ++ " -ohi " ++ hiname
            ifIO (verbose flags) $
	        putStr ("GHC:\n"++ghcp++"\n")
	    writeFile mname ghcp
	    ifIO (ghcComp flags) $ do
	        makeDirPath oname
		ifIO (verbose flags) $
		    putStr ("exec: "++cmd++"\n")
	        rc <- system cmd
		ifIO (keepGhc flags) $
		  do
                    system ("mv " ++ mname ++ " " ++ hsname)
--		removeFile tname `catch` const (return ())
	        removeFile mname `catch` const (return ())
		ifIO (rc /= ExitSuccess) $
		    exitWith rc

mkStartEnv :: Maybe String -> Flags -> [CInterface] -> IO Env
mkStartEnv op flags ifcs = 
    let g = [ (cSignId s, cGlobSignVars s) | CInterface _ s <- ifcs ]
        is = concat (scc g)
        ifcs' = map getIfc is
        getIfc :: Id -> CInterface
        getIfc i = head ([ ifc | ifc@(CInterface _ s) <- ifcs,
                                 cSignId s == i] ++ internalError "mkStartEnv")
    in  --traces (ppReadable (g, is)) $
        case typeCheckInterfaces (redSteps flags) ifcs' of
        Right r -> return r
        Left msg -> messageExit serror (prEMsg msg++"\n")

mkStartEnvI :: Maybe String -> Flags -> [IInterface] -> IO Env
mkStartEnvI op flags ifcs = 
        case typeCheckInterfacesI (redSteps flags) ifcs of
        Right r -> return r
        Left msg -> messageExit serror (prEMsg msg++"\n")

doLink :: Flags -> String -> IO ()
doLink flags name = do
    let (tbl, fname) = hmkFString preStrTable name
        isSel es ls (CSelect (CApply e []) li) = isSel es ls (CSelect e li)
        isSel es ls (CSelect (CVar ei) li) = getIdString ei == es && getIdString li == ls
	isSel _ _ _ = False
    (ifc, _, _) <- getInterface flags (ifcPath flags) (mkId noPosition fname) preStrTable
    ns <- getObjInterfaces flags tbl (ifcPath flags) [mkId noPosition fname] []
    ifIO (verbose flags) $ putStrLn $ "Linking modules " ++ show ns
    case ifc of
        CInterface _ (CSign _ (CApply io [(_, un)])) | 
          isSel "System$IO" "IO" io -> do
	  ghccomp <- getEnvDef "GHC" "ghc"
	  let name' = hIdV name
	      mname = mkModName name
	      tmp = "Main__.hs"
	      tmpt = "Main__.hi"
	      runtime = map ((cayenneDir flags ++ "/System/") ++) ["PTS.o"]
	      os = map (\n -> insertMod (revDrop (length ifSuffix) n ++ oSuffix)) ns
	      inc = dirName (makeFileNameS name)
	      cmd = ghccomp ++ " "++unwords (("-i"++inc):"-o":oFile flags:tmp:runtime++os)
          writeFile tmp ("import "++mname++"\nmain :: IO ()\nmain = "++name'++"\n")
	  ifIO (verbose flags) $
	      putStr ("exec: " ++ cmd ++ "\n")
	  system cmd
	  removeFile tmp
	  removeFile tmpt
	  return ()
	CInterface _ sign -> 
	  messageExit serror ("Top level expression with wrong type: " ++ ppReadable sign)
	_ -> internalError "doLink"

makeDirPath :: String -> IO ()
makeDirPath s =
    let dirs = init (chopList (breakAt '/') s)
        paths = scanl1 (\x y -> x ++ "/" ++ y) dirs
	createDirectory' s = createDirectory s `catch` \ _ -> return ()
    in  mapM_ createDirectory' paths

srcSuffix = "cy"
agdaSuffix = "agda"
hsSuffix = "hs"
hiSuffix = "hi"
ifSuffix = "ci"
ifSuffixBin = "cb"
lmlSuffix = "m"
oSuffix = "o"

parseSrc flags fn inp =
    chkParse pProgram ErrProgram (lexStart (agda flags, haskell flags) fn preStrTable inp)

parseIface tbl fn inp =
    chkParse pInterface ErrInterface (lexStart (False, False) fn tbl inp)

chkParse :: (Parser [Token] (a, StrTable)) -> (EMsg -> a) -> [Token] -> (a, StrTable)
chkParse p ep ts =
    case parse p ts of
	Right ((m,_):_) -> m
        Left  (ss,ts)   -> let (Token _ (L_eof tbl)) = last ts
			   in  (ep (errSyntax (filter (not . null ) ss) ts), tbl)

--

expandInclude :: Flags -> String -> IO String
expandInclude flags s = do
    ls <- mapM (expandLine (cayenneDir flags)) (lines s)
    return (unlines ls)

expandLine :: String -> String -> IO String
expandLine dir ('#':'i':'n':'c':'l':'u':'d':'e':c:cs) | isSpace c = do
    let name = dir ++ "/INCLUDE/" ++ 
               reverse (dropWhile isSpace (reverse (dropWhile isSpace cs)))
    inc <- readFile name
    return (map (\c->if c=='\n' then ' ' else c) inc)
expandLine _ l = return l

--

getInterfaces :: Flags -> StrTable -> [String] -> [Id] -> [Id] -> IO [CInterface]
getInterfaces flags tbl path [] _ = return []
getInterfaces flags tbl path (i:is) done = do
    if i `elem` done then
        getInterfaces flags tbl path is done
     else do
        (ifc, _, tbl') <- getInterface flags path i tbl
	case ifc of
	 ErrInterface msg -> messageExit serror (prEMsg msg++"\n")
	 _ -> do
	    ifcs <- getInterfaces flags tbl' path (is ++ cGlobIfcVars ifc) (i:done)
	    return (ifc:ifcs)

getInterfacesI :: Flags -> StrTable -> [String] -> [Id] -> [Id] -> IO [(IInterface, String)]
getInterfacesI flags tbl path [] _ = return []
getInterfacesI flags tbl path (i:is) done = do
    if i `elem` done then
        getInterfacesI flags tbl path is done
     else do
        (ifc@(IInterface is' _), name, tbl') <- getInterfaceI flags path i tbl
	ifcs <- getInterfacesI flags tbl' path (is ++ is') (i:done)
	return ((ifc, name):ifcs)

getObjInterfaces :: Flags -> StrTable -> [String] -> [Id] -> [Id] -> IO [String]
getObjInterfaces flags tbl path [] _ = return []
getObjInterfaces flags tbl path (i:is) done = do
    if i `elem` done then
        getObjInterfaces flags tbl path is done
     else do
        (ifc, n, tbl') <- getInterface flags path i tbl
	case ifc of
	 CInterface is' _ -> do
	    ns <- getObjInterfaces flags tbl' path (is ++ is') (i:done)
	    return (n:ns)
	 ErrInterface msg -> messageExit serror (prEMsg msg++"\n")

getInterface :: Flags -> [String] -> Id -> StrTable -> IO (CInterface, String, StrTable)
getInterface flags path i tbl = do
    let s = makeFileName i ++ "." ++ ifSuffix
        f dir io = do 
            let name = dir ++ "/" ++ s
	    file <- readFile name
            ifIO (verbose flags) $
                putStr ("read "++name++"\n")
	    return (file, name)
	  `catch` \ _ -> io
    (str, name) <- foldr f (messageExit serror (prPosition (getIdPosition i) ++ 
                        ", Cannot find interface for `"++getIdString i++"'\n")) path
    case parseIface tbl s str of
	(ErrInterface msg, _) -> messageExit serror (prEMsg msg++"\n")
	(ifc, tbl) -> return (ifc, name, tbl)

getInterfaceI :: Flags -> [String] -> Id -> StrTable -> IO (IInterface, String, StrTable)
getInterfaceI flags path i tbl = do
    let s = makeFileName i ++ "." ++ ifSuffixBin
        f dir io = do 
            let name = dir ++ "/" ++ s
	    file <- readFile name
            ifIO (verbose flags) $
                putStr ("read "++name++"\n")
	    return (file, name)
	  `catch` \ _ -> io
    (file, name) <- foldr f (messageExit serror (prPosition (getIdPosition i) ++ 
                        ", Cannot find interface for `"++getIdString i++"'\n")) path
    case iInterfaceFromBytes tbl file of
	(tbl, ifc) -> return (ifc, name, tbl)

makeFileName = makeFileNameS . getIdString
makeFileNameS = map (\c->if c=='$' then '/' else c)
insertMod s = let (a, b) = span (/= '/') (reverse s) in reverse (a ++ "M" ++ b)

genInterface dst name (IModule (i, (t, e, con))) = do
    now <- getClockTime >>= toCalendarTime
    let s = dst ++ "/" ++ makeFileName i ++ "." ++ ifSuffix
	sb = dst ++ "/" ++ makeFileName i ++ "." ++ ifSuffixBin
        ifc = IInterface (iGVars t `union` iGVars e) (i, (toUId i 1, t, if con then Just e else Nothing))
	gen = "-- Generated from `" ++ name ++ "' on " ++ calendarTimeToString now ++ "\n"
    makeDirPath s
    writeFile s (gen ++ ppReadable (ifcIToC False ifc))
    writeFile sb (iInterfaceToBytes ifc)

--

ifIO :: Bool -> IO a -> IO a
ifIO c s = if c then s else return undefined

serror = "error"

hasSuf suf name = length name > length suf && revTake (length suf) name == suf

dropSuf s = if '.' `elem` s then (reverse . tail . dropWhile (/= '.') . reverse) s else s

message :: String -> String -> IO ()
message ekind emsg =
	getProgName >>= \ prog ->
	hPutStr stderr (baseName prog++": Compilation "++ekind++"s:\n"++emsg)

messageExit :: String -> String -> IO a
messageExit ekind emsg =
	message ekind emsg >>
	exitWith (ExitFailure 1)

baseName = reverse . takeWhile (/= '/') . reverse
dirName = reverse . dropWhile (/= '/') . reverse

decodeArgs :: String -> String -> [String] -> Either String (Flags, [String])
decodeArgs prog cd args =
    if "-help" `elem` args then
	Left (helpMessage prog)
    else
	case decodeFlags args ([], defaultFlags cd) of
	([], flags, nonflags) -> Right (flags, nonflags)
	(msg:_, _, _) -> Left (msg ++ usage prog)

usage prog = "Usage: "++prog++" [flags] file.cy\n       "++prog++" -help, to get help\n"

data Flags = Flags { 
	verbose :: Bool,
	extraVerbose :: Bool,
	eval :: Bool,
	tcheck :: Bool,
        lml :: Bool,
        lmlComp :: Bool,
        keepLml :: Bool,
	iface :: Bool,
	dstDir :: String,
	ifcPath :: [String],
	cayenneDir :: String,
	oFile :: String,
	wNoMatch :: Bool,
	redSteps :: Int,
	erase :: Bool,
        agda :: Bool,
        haskell :: Bool,
	ghc :: Bool,
	ghcComp :: Bool,
	keepGhc :: Bool
	}

defaultFlags cayennedir = Flags { 
	verbose = False,
	extraVerbose = False,
	eval = False,
	tcheck = True,
        lml = False,
        lmlComp = False,
        keepLml = False,
	iface = True,
	dstDir = ".",
	ifcPath = [cayennedir, "."],
	cayenneDir = cayennedir,
	oFile = "a.out",
	wNoMatch = True,
	redSteps = 25000,
	erase = True,
        agda = False,
        haskell = False,
	ghc = True,
	ghcComp = True,
	keepGhc = False
	}

decodeFlags :: [String] -> ([String], Flags) -> ([String], Flags, [String])
decodeFlags ("-i":dir:ss) (bad, flags) =
    decodeFlags ss (bad, flags { cayenneDir = dir })
decodeFlags ("-d":dir:ss) (bad, flags) =
    decodeFlags ss (bad, flags { dstDir = dir })
decodeFlags ("-o":file:ss) (bad, flags) =
    decodeFlags ss (bad, flags { oFile = file })
decodeFlags ("-p":dirs:ss) (bad, flags) =
    decodeFlags ss (bad, flags { ifcPath = makePath dirs })
decodeFlags ("-steps":ssteps:ss) (bad, flags) | mnsteps /= Nothing =
    decodeFlags ss (bad, flags { redSteps = fromJust mnsteps })
  where mnsteps = mread ssteps
decodeFlags (('-':s):ss) (bad, flags) =
    decodeFlags ss $
    if take 3 s == "no-" then
        case lookup (drop 3 s) flagsTable of
	Nothing -> (badflag s bad, flags)
	Just f -> (bad, f flags False)
    else
        case lookup s flagsTable of
	Nothing -> (badflag s bad, flags)
	Just f -> (bad, f flags True)
decodeFlags ss (bad, flags) = (bad, flags, ss)

makePath ss = chopList (breakAt ':') ss

mread s = 
    case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

flagsTable = [(s,fs) | (s,fs,_) <- flagdescs]
flagdescs = [
	("help",		noop,			"generate help message"),
	("verbose",		\f x->f{verbose=x},	"be talkative"),
	("v",			\f x->f{verbose=x, extraVerbose=verbose f},	"same as -fverbose"),
	("eval",		\f x->f{eval=x},	"evaluate and print result"),
	("tcheck",		\f x->f{tcheck=x},	"type check program"),
	("code",		\f x->f{lml=x},		"generate code"),
	("interface",		\f x->f{iface=x},	"generate interface file"),
	("lml",			\f x->f{lml=x},		""),
	("ghc",			\f x->f{ghc=x},		""),
	("lmlComp",		\f x->f{lmlComp=x},	"compile LML"),
	("ghcComp",		\f x->f{ghcComp=x},	"compile GHC"),
	("m",			\f x->f{keepLml=x},	"keep LML"),
	("h",			\f x->f{keepGhc=x},	"keep GHC"),
	("erase",		\f x->f{erase=x},	"erase types"),
	("Wno-match",		\f x->f{wNoMatch=x},	"warn about missing case branches"),
        ("agda",		\f x->f{agda=x},	"use Agda compatible syntax"),
        ("haskell",		\f x->f{haskell=x},	"use (mostly) Haskell compatible syntax")
	]

noop f _ = f

badflag s bad | s `elem` ignore = bad
	      | otherwise = ("Unrecognized flag: -"++s++", use -help to get a list\n") : bad

ignore = ["debug", "full"]

helpMessage prog = unlines ([
	"",
	"Usage: "++prog++" [flags] file." ++ srcSuffix   ++ "       -- to compile a file",
	"    or "++prog++" [flags] moduleId      -- to link and generate a binary",
	"    or "++prog++" [flags] file." ++ ifSuffixBin ++ "       -- to view a binary interface file",
	"",
	"Look at http://www.cs.chalmers.se/~augustss/cayenne/ for more information.",
	"",
	"Most flags may be preceded by a `no-' to reverse the effect.",
	"Flags later on the command line overrides earlier ones.",
	"Compiler flags:",
	"-i dir                directory for Cayenne files",
	"-d dir                destination for generated files",
	"-o file               name of executable",
	"-p path               search path for interface files",
	"-steps nstep          set the number of reduction steps",
	""
	 ] ++ 
	describe flagdescs)
  where describe fs = sort [ "-" ++ f ++ replicate (20 - length f) ' ' ++ " " ++ desc | (f, _, desc) <- fs]

getWarn :: IExpr -> [EMsg]
getWarn (ILit _ (LNoMatch pos)) = [(pos, WNoMatch)]
getWarn (IWarn w _) = [w]
getWarn _ = []

getEnvDef :: String -> String -> IO String
getEnvDef e d = getEnv e `catch` \ _ -> return d

---
-- XXX
--getPid :: IO Int
--getPid = return 42
