module example$tinterp_pe$prelude =
open example$tinterp_pe$abssyn use * in
open example$tinterp_pe$aux use (==>) in

struct

prelude :: Env
prelude = extends "+" (TInt ==> (TInt ==> TInt)) System$Int.(+) (
          extends "-" (TInt ==> (TInt ==> TInt)) System$Int.(-) (
          extends "True" TBool System$Bool.True (
          extends "False" TBool System$Bool.False (
          extends "<=" (TInt ==> (TInt ==> TBool)) System$Int.(<=)
          emptyEnv))))
