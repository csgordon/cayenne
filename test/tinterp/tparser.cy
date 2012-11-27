module example$tinterp$tparser = 

open System$List use * in
open System$String use String in
open System$StringUtil use (==) in
open System$Bool use * in
open System$Unit use Unit, unit in
open System$Maybe use Maybe, Just, Nothing in
open example$tinterp$abssyn use * in
open example$tinterp$lex use * in
open example$parselib Lex use * in

struct

type EExpr (s :: TEnv) = sig { t :: Type; e :: Expr s t }

type EParser = (s :: TEnv) -> Parser (EExpr s)

eexpr :: (s :: TEnv) -> (t :: Type) -> Expr s t -> EExpr s
eexpr s t' e' = struct { t = t'; e = e' }

parseType :: Parser Type
parseType =
    success TInt  << kw "Int"
 || success TBool << kw "Bool"
 || paren (success TArrow * parseType << kw "->" * parseType)

parseSym :: Parser String
parseSym = match isSym "symbol" 

parseId :: Parser String
parseId = match isId "id"

kw :: String -> Parser String
kw s = match (isKW s) ("`" ++ s ++ "'")

paren :: (a :: #) |-> Parser a -> Parser a
paren p = match isLP "`('" >> p << match isRP "`)'"

checkType :: (t1, t2 :: Type) -> Parser (EqType t1 t2)
checkType t1 t2 = case eqType' t1 t2 of
                     (Just p)  -> success p
                     (Nothing) -> fail ("Type error. Inferred " ++ showType t1 ++ ", context demands " ++ showType t2)

checkConvertExpr :: (t1, t2 :: Type) -> (s :: TEnv) -> Expr s t1 -> Parser (Expr s t2)
checkConvertExpr t1 t2 s e = do Monad_Parser
   (p :: EqType t1 t2) <- checkType t1 t2
   return (substType (Expr s) t1 t2 p e)


checkConvertE :: (s :: TEnv) -> (t :: Type) -> EExpr s -> Parser (Expr s t)
checkConvertE s t e = checkConvertExpr e.t t s e.e
