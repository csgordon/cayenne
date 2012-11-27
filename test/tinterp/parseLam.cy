module example$tinterp$parseLam = 

open System$List use * in
open System$String use String in
open example$tinterp$abssyn use * in
open example$tinterp$aux use * in
open example$tinterp$lex use * in
open example$tinterp$tparser use * in
open example$parselib Lex use * in

struct

parseLam :: EParser -> EParser
parseLam parseExpr s = do Monad_Parser
   kw "\\"
   (x :: String) <- parseId
   kw "::"
   (a :: Type) <- parseType
   kw "->"
   parseLam2 parseExpr x a s

parseLam2 :: EParser -> Symbol -> Type -> EParser
parseLam2 parseExpr x a s = do Monad_Parser
   let s' :: TEnv = extend s x a
   (e :: EExpr s') <- parseExpr s'
   return (eexpr s (TArrow a e.t) (lam a e.t x e.e))
