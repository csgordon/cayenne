module example$tinterp$prelude =
open example$tinterp$abssyn use * in
open example$tinterp$aux use (==>) in

struct

prelude :: Env
prelude = extends "+" (TInt ==> (TInt ==> TInt)) System$Int.(+) (
          extends "-" (TInt ==> (TInt ==> TInt)) System$Int.(-) (
          extends "True" TBool System$Bool.True (
          extends "False" TBool System$Bool.False (
          extends "<=" (TInt ==> (TInt ==> TBool)) System$Int.(<=)
          emptyEnv))))
