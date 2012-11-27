module example$interp$ienv =
open example$interp$abssyntype use Type, TInt, Decode in
open System$Bool use Bool, if, IF in
open System$Char use Char, (==) in

struct

type Symbol = Char

type TEnv = Symbol -> Type

emptyTEnv :: TEnv
emptyTEnv = \ s -> TInt

extendT :: TEnv -> Symbol -> Type -> TEnv
extendT s x t = \ y -> if (x == y) t (s y)

type VEnv (s :: TEnv) = (x :: Symbol) -> Decode (s x)

emptyVEnv :: VEnv emptyTEnv
emptyVEnv = \ s -> 0

extendV :: (s :: TEnv) ->
             (r :: VEnv s) ->
             (x :: Symbol) ->
             (t :: Type) ->
             (v :: Decode t) ->
	     (VEnv (extendT s x t))

extendV s r x t v = \ y -> IF (x == y) v (r y)
