module example$comb =
#include Prelude
open System$Logic use * in

struct

data Exp 
       = App Exp Exp
       | Lam Sym Exp
       | Var Sym
type Sym = String

showExp :: Exp -> String
showExp (App f a) = "(" ++ showExp f ++ " " ++ showExp a ++ ")"
showExp (Lam x e) = "(\\" ++ x ++ "." ++ showExp e ++ ")"
showExp (Var s) = s

(==) :: Sym -> Sym -> Bool
(==) x y = equal System$Char.(==) x y

LamFree :: Exp -> #
LamFree (App f a) = LamFree f /\ LamFree a
LamFree (Lam _ _) = Absurd
LamFree (Var _) = Truth

type LamFreeExp = sig exp :: Exp
                      lf  :: LamFree exp

abstractVars :: Exp -> LamFreeExp
abstractVars e@(Var _) = struct { exp = e; lf = truth }
abstractVars (App f a) =
    let f' = abstractVars f
        a' = abstractVars a
    in  struct exp = App f'.exp a'.exp
               lf  = f'.lf & a'.lf
abstractVars (Lam x e) =
    let e' = abstractVars e
    in  abstractVar x e'.exp e'.lf

abstractVar :: Sym -> (e :: Exp) -> LamFree e -> LamFreeExp
abstractVar s (App f a) (lf & la) =
    let f' = abstractVar s f lf
        a' = abstractVar s a la
    in  struct exp = App (App S f'.exp) a'.exp
               lf  = (truth & f'.lf) & a'.lf
abstractVar s (Lam _ _) l = absurd l
abstractVar s e@(Var x) l = 
    if (s == x)
       (struct {exp = I; lf :: LamFree exp = truth})
       (struct {exp = App K e; lf :: LamFree exp = truth & l})

S = Var "S"
K = Var "K"
I = Var "I"
