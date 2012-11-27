module example$subtypes =
#include Prelude
struct

type Point = sig
        x :: Integer
        y :: Integer

type ColorPoint = sig
        x :: Integer
        y :: Integer
        color :: Integer

data PointFields = point | colorpoint

PointType :: PointFields -> #
PointType (point) = Point
PointType (colorpoint) = ColorPoint

-- Overloaded point
OPoint = (f :: PointFields) |-> PointType f

mkColorPoint :: ColorPoint -> OPoint
mkColorPoint cp |(point) = struct { x = cp.x; y = cp.y }
mkColorPoint cp |(colorpoint) = cp

distance :: PointType point -> Integer
distance p = abs (p.x) + abs (p.y)

abs :: Integer -> Integer
abs x = if (x < 0) (negate x) x

c = mkColorPoint (struct { x = 1; y = 2; color = 3 })

p = struct { x = 3; y = 4 }

i = distance c + distance p
