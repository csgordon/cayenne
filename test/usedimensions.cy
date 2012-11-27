module example$usedimensions =
open example$dimensions use * in

struct
l = length 100.0

t = time 5.0

s :: Speed
s = l / t

l2 = s * t + l

-- err = l + t
