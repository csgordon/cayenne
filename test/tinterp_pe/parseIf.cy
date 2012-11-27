module example$tinterp_pe$parseIf = 

open System$List use * in
open System$String use String in
open example$tinterp_pe$abssyn use * in
open example$tinterp_pe$aux use * in
open example$tinterp_pe$lex use * in
open example$tinterp_pe$tparser use * in
open example$parselib Lex use * in

struct

parseIf :: EParser -> EParser
parseIf parseExpr s = do Monad_Parser
   kw "if"
   (b :: EExpr s) <- parseExpr s
   (b' :: Expr s TBool) <- checkConvertExpr b.t TBool s b.e
   kw "then"
   (e1 :: EExpr s) <- parseExpr s
   kw "else"
   parseIf2 parseExpr s b' e1

parseIf2 :: EParser -> (s :: TEnv) -> Expr s TBool -> EExpr s -> Parser (EExpr s)
parseIf2 parseExpr s b' e1 = do Monad_Parser
   (e2 :: EExpr s) <- parseExpr s
   (e2' :: Expr s e1.t) <- checkConvertE s e1.t e2
   return (eexpr s e1.t (if e1.t b' e1.e e2'))
