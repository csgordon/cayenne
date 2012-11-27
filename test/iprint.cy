module example$interp$iprint =
open example$interp$abssyntype use * in
open example$interp$abssyn use * in
open System$String use * in
struct

print :: (t :: Type) -> Decode t -> String
print (TInt) i = System$Int.show i
print (TBool) b = System$Bool.show b
print (TArrow _ _) f = "<function>"
