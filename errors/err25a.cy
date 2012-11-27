module err$err25a = 
{
data Nat = Zero | Succ Nat;
data T   = tt;
data F = ;

eqNat :: Nat -> Nat -> #;
eqNat (Zero) (Zero) = T;
eqNat (Zero) (Succ y) = F;
eqNat (Succ x) (Zero) = F;
eqNat (Succ x) (Succ y) = eqNat x y;

y :: Nat;
y = Succ y;

x :: Nat;
x = y;

refl :: (n::Nat) -> eqNat n n ;
refl (Zero) = tt;
refl (Succ n) = refl n;


test2 :: eqNat x y;
test2 x = refl x;
};

