module System$Void =
struct
Void :: #
Void = (data )

void :: (a :: #) |-> Void -> a
void v = case v of { }
