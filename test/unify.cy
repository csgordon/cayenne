module example$unify = 
struct

data Absurd = 

data Truth = one

type Not a = a -> Absurd

type And a b = sig { fst :: a; snd :: b; }

data Or a b = Inl a | Inr b

Rel = \ (a :: #) -> a -> a -> #

data Nat = Zero | Succ Nat

eqNat :: Nat -> Nat -> #
eqNat (Zero)   (Zero)    = Truth
eqNat (Succ n) (Succ n') = eqNat n n'
eqNat _        _         = Absurd

type Id = Nat

data Term = Var Id | Leaf | Node Term Term

eqTerm :: Term -> Term -> #
eqTerm (Var n)    (Var n')     = eqNat n n'
eqTerm (Leaf)     (Leaf)       = Truth
eqTerm (Node l r) (Node l' r') = And (eqTerm l l') (eqTerm r r')
eqTerm _          _            = Absurd

varInTerm :: Term -> Id -> #
varInTerm (Var v) n    = eqNat n v
varInTerm (Leaf)  n    = Absurd
varInTerm (Node l r) n = Or (varInTerm l n) (varInTerm r n)

type Subst = Id -> Term

emptySubst :: Subst = Var

substitute :: Subst -> Term -> Term
substitute s (Var n)    = s n
substitute s (Leaf)     = Leaf
substitute s (Node l r) = Node (substitute s l) (substitute s r)

comp :: Subst -> Subst -> Subst
comp s s' = \ (v::Id) -> substitute s (s' v)

type Unifier (s::Subst) (t1::Term) (t2::Term) = eqTerm (substitute s t1) (substitute s t2);

{-
OrE :: Rel Term -> Term -> Term -> #;
OrE r x y = Or (r x y) (eqTerm x y);

xLT :: Rel Term;
xLT _ (Var _) = Absurd;
xLT _ (Leaf)  = Absurd;
xLT (Var _) (Node _ _) = Truth
xLT (Leaf) (Node _ _) = Truth
xLT (Node _ _) (Node l r) = Or (OrE xLT x t1) (OrE xLT x t2);
-}

Assumptions = 
  sig {
   lt :: Rel Term;
   le :: Rel Term = \ (x::Term) -> \ (y::Term) -> Or (lt x y) (eqTerm x y);

   irr :: (t1::Term) -> (t2::Term) -> (h::lt t1 t2) ->  Not (eqTerm t1 t2);
   monoT1 :: (t1::Term) -> (t2::Term) -> (t3::Term) -> (h::le t1 t2) -> lt t1 (Node t2 t3);
   monoT2 :: (t1::Term) -> (t2::Term) -> (t3::Term) -> (h::le t1 t3) -> lt t1 (Node t2 t3);
   substIsSubst :: (s::Subst)->(t1::Term)->(t2::Term)->(h::eqTerm t1 t2) -> eqTerm (substitute s t1) (substitute s t2);
  }

tmpTheory = \ (as::Assumptions) -> struct {

 lemma0 :: (v::Id) -> (t::Term) -> (varInTerm t v) -> (s::Subst) -> Or (eqTerm (Var v) t) (as.lt (s v) (substitute s t));
 lemma0 _ (Var _) h _ = Inl h;
 lemma0 _ (Leaf) h _ = case h of {};
 lemma0 v (Node l r) h s =
       case h of {
       (Inl x) ->  
           case lemma0 v l x s of {
           (Inl x') -> Inr (as.monoT1 (s v) (substitute s l) (substitute s r) 
                          (Inr (as.substIsSubst s (Var v) l x')));
           (Inr y)  -> Inr (as.monoT1 (s v) (substitute s l) (substitute s r) 
                          (Inl y));
	   };
       (Inr y) -> 
           case lemma0 v r y s of {
           (Inl x)  -> Inr (as.monoT2 (s v) (substitute s l) (substitute s r) 
                          (Inr (as.substIsSubst s (Var v) r x)));
           (Inr y') -> Inr (as.monoT2 (s v) (substitute s l) (substitute s r) 
                          (Inl y'));
	   }
       };


 lemma0' :: (v::Id) -> (t::Term) -> (varInTerm t v) -> (Not (eqTerm (Var v) t)) -> (s::Subst) -> Not (Unifier s (Var v) t);
 lemma0' v t h h1 s =
    case lemma0 v t h s of {
       (Inl x) -> case h1 x of {};
       (Inr y) -> as.irr (s v) (substitute s t) y;
    };


 }
