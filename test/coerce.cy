module example$coerce =
#include Prelude
struct

concrete
SigBig = sig
   x :: Integer = 1
   y :: Integer
   z :: Integer

concrete
SigSmall = sig
   x :: Integer
   y :: Integer

recBig :: SigBig
recBig = struct
   concrete x = 1
   y = 2
   z = 3

-- Coerce the record to hide the value of x 
-- and to hide z completely.
recSmall :: SigSmall
recSmall = recBig :: SigSmall
