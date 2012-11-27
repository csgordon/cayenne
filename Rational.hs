module Rational(prRational, tinyDouble, tinyFloat, hugeDouble, hugeFloat, tiny, huge) where

-- Convert a Rational to a string that looks like a floating point number,
-- but without converting to any floating type (because of the possible overflow).
prRational :: Int -> Rational -> String
prRational n r =
    if r == 0 then
    	"0.0"
    else if r < 0 then
	'-' : prRational n (-r)
    else
	let (r', e) = normalize r
	in  prR n r' e

startExpExp = 4 :: Int

-- make sure 1 <= r < 10
normalize :: Rational -> (Rational, Int)
normalize r = if r < 1 then case norm startExpExp (1 / r) 0 of (r', e) -> (10 / r', -e-1) else norm startExpExp r 0
	where norm :: Int -> Rational -> Int -> (Rational, Int)
	      -- Invariant: r*10^e == original r
	      norm 0  r e = (r, e)
	      norm ee r e =
		let n = 10^ee
		    tn = 10^n
		in  if r >= tn then norm ee (r/tn) (e+n) else norm (ee-1) r e

drop0 "" = ""
drop0 (c:cs) = c : reverse (dropWhile (=='0') (reverse cs))

prR :: Int -> Rational -> Int -> String
prR n r e | r <   1 = prR n (r*10) (e-1)		-- final adjustment
prR n r e | r >= 10 = prR n (r/10) (e+1)
prR n r e0 =
	let s = show ((round (r * 10^n))::Integer)
	    e = e0+1
	in  if e > 0 && e < 8 then
		take e s ++ "." ++ drop0 (drop e s)
	    else if e <= 0 && e > -3 then
	        "0." ++ take (-e) (repeat '0') ++ drop0 s
	    else
	        head s : "."++ drop0 (tail s) ++ "e" ++ show e0

------------

-- Compute smallest and largest floating point values.
tiny :: (RealFloat a) => a
tiny =
	let (l, _) = floatRange x
	    x = encodeFloat 1 (l-1)
	in  x
huge :: (RealFloat a) => a
huge =
	let (_, u) = floatRange x
	    d = floatDigits x
	    x = encodeFloat (floatRadix x ^ d - 1) (u - d)
	in  x

tinyDouble = tiny :: Double
tinyFloat  = tiny :: Float
hugeDouble = huge :: Double
hugeFloat  = huge :: Float
