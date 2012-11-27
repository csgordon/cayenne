module example$div =
#include Prelude

struct

data Truth = truth
data Absurd =

concrete
NonZeroInteger = sig { i :: Integer; nonZero :: case i of { 0 -> Absurd; _ -> Truth } }

div :: Integer -> NonZeroInteger -> Integer
div x y = x `quot` y.i

myFunction :: Integer -> Integer -> Integer
myFunction a 0 = 0
myFunction a b = div a { i = b; nonZero = truth }
				   

