module example$congr =
open System$Error use undefined in
open System$HO use id in
open System$Tuples use * in
open System$Bool use * in
open System$Unit use * in
open System$List use List, Nil, (:) in
open example$xlogic use * in
open example$equiv use * in

struct

--------
-- The Eq "class", with congruence proof
concrete
--Eq :: (a :: #) -> #1.0
--Eq a = sig
Eq = \ (a :: #) -> sig
    (==) :: a -> a -> Bool
    equiv :: Equiv (LiftBin (==))

isoEquiv :: (a :: #) |-> 
	 (p, q :: Rel a) ->
	 ((x, y :: a) -> p x y <=> q x y) -> Equiv p -> Equiv q
isoEquiv p q iso eqp = struct
    refl x = (iso x x).impR (eqp.refl x)
    symm x y lp = (iso y x).impR (eqp.symm x y ((iso x y).impL lp))
    trans x y z lp lq = (iso x z).impR
	(eqp.trans x y z ((iso x y).impL lp) ((iso y z).impL lq))
    cong x y lp = eqp.cong x y ((iso x y).impL lp)

--------
-- Equality on Unit
Eq_Unit :: Eq Unit
Eq_Unit = struct 
    (==) (unit) (unit) = True

    equiv = struct
	refl (unit) = truth
	symm (unit) (unit) p = p
	trans (unit) (unit) (unit) p q = p
        cong (unit) (unit) p fx = fx
	

--------
-- Equality on Bool
Eq_Bool :: Eq Bool
Eq_Bool = struct
    (==) (False) (False) = True
    (==) (True)  (True)  = True
    (==) _       _       = False

    equiv = struct
	refl (False) = truth
	refl (True)  = truth
	symm (False) (False) p = p
	symm (False) (True)  p = absurd p
	symm (True)  (False) p = absurd p
	symm (True)  (True)  p = p
--	symm _       _       p = absurd p
	trans (False) (False) (False) p q = q
	trans (False) (False) (True)  p q = absurd q
	trans (False) (True)  _       p q = absurd p
	trans (True)  (False) _       p q = absurd p
	trans (True)  (True)  (False) p q = absurd q
	trans (True)  (True)  (True)  p q = q
        cong (True) (True) p fx = fx
        cong (False) (False) p fx = fx
        cong (True) (False) p fx = absurd p
        cong (False) (True) p fx = absurd p


--------

private
liftAndL :: (x,y::Bool) -> Lift (x && y) -> Pair (Lift x) (Lift y)
liftAndL (False) _       a = absurd a
liftAndL (True)  (False) a = absurd a
liftAndL (True)  (True)  t = (t, t)

private
liftAndR :: (x,y::Bool) -> Pair (Lift x) (Lift y) -> Lift (x && y)
liftAndR (False) _       (a, _) = a
liftAndR (True)  (False) (_, a) = a
liftAndR (True)  (True)  (t, _) = t

-- Equality on pairs.
Eq_Pair :: (a,b :: #) |-> Eq a -> Eq b -> Eq (Pair a b)
Eq_Pair eqa eqb = struct
    (==) (x, x') (y, y') = eqa.(==) x y && eqb.(==) x' y'

    private
    eq :: Pair a b -> Pair a b -> #
    eq (x, x') (y, y') = Pair (LiftBin eqa.(==) x y) (LiftBin eqb.(==) x' y')

    private
    eqEq :: (x,y::Pair a b) -> eq x y <=> Lift (x == y)
    eqEq (x, x') (y, y') = struct
	impR p = liftAndR (eqa.(==) x y) (eqb.(==) x' y') p
	impL p = liftAndL (eqa.(==) x y) (eqb.(==) x' y') p

    private
    equivEq :: Equiv eq
    equivEq = struct
	refl (x, x') = (eqa.equiv.refl x, eqb.equiv.refl x')
	symm (x, x') (y, y') (pxy, pxy') = 
            (eqa.equiv.symm x y pxy, eqb.equiv.symm x' y' pxy')
	trans (x, x') (y, y') (z, z') (pxy, pxy') (pyz, pyz') = 
	    (eqa.equiv.trans x y z pxy pyz, eqb.equiv.trans x' y' z' pxy' pyz')
        cong |F (x, x') (y, y') (pxy, pxy') fxx' = 
          eqb.equiv.cong|(\ (v :: b) -> F (y,v)) x' y' pxy'
         (eqa.equiv.cong|(\ (v :: a) -> F (v,x')) x y pxy fxx')
            
    equiv = isoEquiv eq (LiftBin (==)) eqEq equivEq

--------
-- Equality on lists.
Eq_List :: (a :: #) -> Eq a -> Eq (List a)
Eq_List a eqa = struct
    (==) :: List a -> List a -> Bool
    (==) (Nil)  (Nil)  = True
    (==) (x:xs) (y:ys) = eqa.(==) x y && xs == ys
    (==) _      _      = False

    private
    eq :: List a -> List a -> #
    eq (Nil)  (Nil)  = Truth
    eq (x:xs) (y:ys) = Pair (LiftBin (eqa.(==)) x y) (eq xs ys)
    eq _      _      = Absurd

    private
    eqEq :: (xs,ys :: List a) -> eq xs ys <=> Lift (xs == ys)
    eqEq (Nil)  (Nil)  = struct impL = id; impR = id
    eqEq (x:xs) (y:ys) = struct
	impR (p, p') = liftAndR (eqa.(==) x y) (xs == ys) (p, (eqEq xs ys).impR p')
	impL p = case liftAndL (eqa.(==) x y) (xs == ys) p of
		 (q, q') -> (q, (eqEq xs ys).impL q')
    eqEq (Nil)  (_:_)  = struct impL = absurd; impR = absurd
    eqEq (_:_)  (Nil)  = struct impL = absurd; impR = absurd

    private
    equivEq :: Equiv eq
    equivEq = struct
	refl :: (xs::List a) -> eq xs xs
	refl (Nil) = truth
	refl (x:xs) = (eqa.equiv.refl x, refl xs)
	symm :: (xs::List a) -> (ys::List a) -> eq xs ys -> eq ys xs
	symm (Nil) (Nil) p = p
	symm (Nil) (_:_) p = absurd p
	symm (_:_) (Nil) p = absurd p
	symm (x:xs) (y:ys) (p,q) = (eqa.equiv.symm x y p, symm xs ys q)
	trans :: (xs::List a) -> (ys::List a) -> (zs::List a) -> 
		 eq xs ys -> eq ys zs -> eq xs zs
	trans (Nil)  (Nil)  (Nil)  p q = p
	trans (Nil)  (Nil)  (_:_)  p q = absurd q
	trans (Nil)  (_:_)  _      p q = absurd p
	trans (_:_)  (Nil)  _      p q = absurd p
	trans (_:_)  (_:_)  (Nil)  p q = absurd q
	trans (x:xs) (y:ys) (z:zs) (pxy,pxys) (pyz,pyzs) = 
	    (eqa.equiv.trans x y z pxy pyz, trans xs ys zs pxys pyzs)

        cong :: (F :: List a -> #) |-> (xs,ys :: List a) -> eq xs ys -> F xs -> F ys
        cong |F (Nil) (Nil) p fx = fx
        cong |F (x:xs) (y:ys) (pxy,pxys) fxxs = 
            cong|(\ (l :: List a) -> F (y:l)) xs ys pxys
           (eqa.equiv.cong|(\ (v :: a) -> F (v:xs)) x y pxy fxxs)
        cong |F (_:_) (Nil) p fx = absurd p
        cong |F (Nil) (_:_) p fx = absurd p

    equiv = isoEquiv eq (LiftBin (==)) eqEq equivEq

--------

