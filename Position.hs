module Position where
import PPrint

data Position = Position String Int Int		-- file, line, pos
		deriving (Eq, Ord, Show)
prPosition (Position f l c) =
    if l<0 && c<0 && f=="" then "Unknown position" else
    let lc = if l<0 then "" else "line "++show3 l ++ (if c < 0 then "" else ", column "++show3 c)
        show3 = show --until ((>=3) . length) (' ':) . show
    in	case f of
	    "" -> lc
	    _ -> "\""++f ++ "\"" ++ (if null lc then "" else ", "++lc)
noPosition = Position "" (-1) (-1)
remPositionFile (Position _ l c) = Position "" l c
filePosition f = Position f (-1) (-1)

getPositionLine (Position _ l _) = l

instance PPrint Position where
    pPrint _ _ p = text (prPosition p)
