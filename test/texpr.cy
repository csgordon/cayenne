module example$interp$texpr =
open System$Char use Char in
open System$Bool use Bool in
open System$Int use Int in

struct

type Symbol = Char

public
data Type = TInt | TBool | TArrow Type Type

type TExprSpec (t :: Type) = case t of
   (TInt)       -> IntExpr
   (TBool)      -> BoolExpr
   (TArrow f t) -> ArrowExpr f t

data TExpr (t::Type) =
   Var Symbol
-- | App (f:: Type) (TExpr (TArrow f t)) (TExpr t)
 | App sig{ f:: Type;  e1:: TExpr (TArrow f t); e2 :: TExpr t}
 | Spec (TExprSpec t)

data IntExpr = 
   IConst Int 
 | IPlus (TExpr TInt) (TExpr TInt)

data BoolExpr = 
   BConst Bool 
 | ILE (TExpr TInt) (TExpr TInt)
 | BAnd (TExpr TBool) (TExpr TBool)

data ArrowExpr (f::Type) (t::Type) = 
   Lam Symbol (TExpr t)


