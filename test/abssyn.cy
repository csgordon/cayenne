module example$interp$abssyn =
open System$Char use Char in
open System$Int use Int in
open System$Bool use Bool, (&&), False, True in
open System$Maybe use Maybe, Just, Nothing in

open example$xlogic use Absurd, absurd, Truth, truth, Lift, AndI, (/\), (&) in

open example$interp$abssyntype use * in
open example$interp$ienv use Symbol in
struct

public
data Expr = 
      Var Symbol
    | App Expr Expr
    | Lam Symbol Type Expr
    | IConst Int
    | BConst Bool
    | IPlus Expr Expr
    | ILE Expr Expr
    | BAnd Expr Expr

