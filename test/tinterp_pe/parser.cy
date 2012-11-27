module example$tinterp_pe$parser = 

open System$List use * in
open System$String use String in
open System$StringUtil use (==) in
open System$Bool use * in
open System$Int use read, Int in
open System$Tuples use Pair, (,), snd in
open System$Unit use Unit, unit in
open System$HO use flip, (·), id in
open System$Maybe use Maybe, Just, Nothing in
open System$MonadUtil use join in
open System$Either use Either, Left, Right, either in
open example$tinterp_pe$abssyn use * in
open example$tinterp_pe$aux use * in
open example$tinterp_pe$lex use * in
open example$tinterp_pe$tparser use * in
open example$tinterp_pe$parseLam use * in
open example$tinterp_pe$parseIf use * in
open example$tinterp_pe$parseFix use * in
open example$parselib Lex use * in
open Monad_Parser use * in

private
struct

public 
parser :: (prelude :: Env) -> String
       -> Either (Pair Int (List String)) (EExpr prelude.tenv)
parser prelude s = 
 -- either id snd · parse (parseExpr prelude.tenv < match isEOF "<EOF>") · lex
   ppp (parse (parseExpr prelude.tenv << match isEOF "<EOF>") (lex s))

ppp :: (a,b,c :: #) |-> Either a (Pair b c) -> Either a c
ppp x = case x of
    (Left e) -> Left e
    (Right (x,y)) -> Right y

parseExpr :: EParser
parseExpr s = 
  join monad (success (applyOps s) * 
              parseExpr1 s * 
              many (success ((,)|String|(EExpr s)) * parseSym * parseExpr s))

parseExpr1 :: EParser
parseExpr1 s = parseLam parseExpr s 
            || parseIf parseExpr s
            || parseFix parseExpr s
            || parseApp s

parseApp :: EParser
parseApp s = join monad (success (applys s) * 
                         parseAExpr s * 
                         many (parseAExpr s))

parseAExpr :: EParser
parseAExpr s = parseVar s || paren (parseExpr s) || parseInt s

parseVar :: EParser
parseVar s = do Monad_Parser
    (x :: String) <- parseId
    return (mkVar s x)

parseInt :: EParser
parseInt s = success (\ (i :: Int) -> eexpr s TInt (iconst|s i)) * match isInt "integer"
     
mkVar :: (s :: TEnv) -> String -> EExpr s
mkVar s x = let t = s x 
            in eexpr s t (var s t x (refType t))

applyOps :: (s :: TEnv) -> EExpr s -> List (Pair String (EExpr s)) 
         -> Parser (EExpr s)
applyOps s e1 (Nil) = success e1
applyOps s e1 ((o,e2):es) =
    apply s (mkVar s o) e1 >>= flip (apply s) e2 >>= flip (applyOps s) es

applys :: (s :: TEnv) -> EExpr s -> List (EExpr s) -> Parser (EExpr s)
applys s f (Nil) = success f
applys s f (a:as) = apply s f a `bind` flip (applys s) as

apply :: (s :: TEnv) -> EExpr s -> EExpr s -> Parser (EExpr s)
apply s f e = do Monad_Parser
   let err :: Parser (EExpr s)
       err = fail ("Type error. Expression of type " ++ showType f.t ++ " used as function (extra argument?)")
   --case f.t of -- XXX
   let ft = f.t
       fe :: Expr s ft = f.e
   case ft of
        (TInt)       -> err -- XXX
        (TArrow a t) -> do Monad_Parser
                (e' :: Expr s a) <- checkConvertE s a e
                return (eexpr s t (app |s a t fe e'))
        _ -> err

