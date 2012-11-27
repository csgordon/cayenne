module example$tinterp$parseFix = 

open System$List use * in
open System$String use String in
open example$tinterp$abssyn use * in
open example$tinterp$aux use * in
open example$tinterp$lex use * in
open example$tinterp$tparser use * in
open example$parselib Lex use * in

struct

parseFix :: EParser -> EParser
parseFix parseExpr s = do Monad_Parser
   kw "fix"
   (x :: String) <- parseId
   kw "::"
   (t :: Type) <- parseType
   kw "->"
   parseFix2 parseExpr x t s

parseFix2 :: EParser -> Symbol -> Type -> EParser
parseFix2 parseExpr x t s = do Monad_Parser
   let s' :: TEnv = extend s x t
   (e :: EExpr s') <- parseExpr s'
   (e' :: Expr s' t) <- checkConvertE s' t e
   return (eexpr s t (fix t x e'))
