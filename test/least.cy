module example$least =
open example$misc use * in
struct
least (sequence :: example$Sequence) (A::Set) ((<=) :: rel A) (p::poset A (<=)) (l::linear A (<=)) =
 open sequence A use Seq, forall, fa1, fa2, exists, ex1, ex2, singl,
                     cons, seqrec
 in 
 private struct

 public concrete
 (==) :: rel A = \ (x::A) -> \ (y::A) -> (x<=y) & (y<=x)

 public concrete
 least (xs::Seq) (x::A) :: Set =
   forall ((<=) x) xs & exists ((==) x) xs

 lem1 (x::A) :: x == x
   = pair (x<=x) (x<=x) (p.fst x) (p.fst x)

 lem2 (x::A) (y::A) (h:: x <= y) (xs::Seq) (h1::forall ((<=) y) xs)
   :: forall ((<=) x) xs =
   (sequence A).lem1 ((<=) y) ((<=) x) 
                     (\ (z::A) -> \ (h2:: y <= z) -> p.snd x y z h h2)
                     xs h1

 lem3 (x::A) (y::A) (xs::Seq) (h::least xs x) (h1::x <= y) 
   :: least (cons y xs) x =
   pair (forall ((<=) x) (cons y xs)) 
        (exists ((==) x) (cons y xs))
        ((fa2 ((<=) x) y xs).snd 
             (pair (x <= y) (forall ((<=) x) xs) h1 h.fst))
        ((sequence A).lem3 ((==) x) y xs h.snd)

 lem4 (x::A) (y::A) (xs::Seq) (h::least xs x) (h1::y <= x) 
   :: least (cons y xs) y =
   pair (forall ((<=) y) (cons y xs)) 
        (exists ((==) y) (cons y xs))
        ((fa2 ((<=) y) y xs).snd
              (pair (y <= y) 
                    (forall ((<=) y) xs) 
                    (p.fst y)
                    (lem2 y x h1  xs h.fst)))
        ((sequence A).lem4 ((==) y) y xs (lem1 y))

 lem5 (x::A) :: least (singl x) x =
   pair (forall ((<=) x) (singl x)) (exists ((==) x) (singl x))
        ((fa1 ((<=) x) x).snd (p.fst x))
        ((ex1 ((==) x) x).snd (lem1 x))   

 public
 problem :: (xs :: Seq) -> sigma A (least xs) =

    let

     P (xs::Seq) :: Set = sigma A (least xs)

     base (x::A) :: P (singl x) =
         struct
            fst :: A = x
--            snd :: least (singl x) x = lem5 x
            snd :: least (singl x) fst = lem5 x

     indStep (x::A) (xs::Seq) (h::P xs) :: P (cons x xs) =
         let u :: A = h.fst
             u1 :: least xs u = h.snd
             rem :: (x <= u) + (u <= x) = l x u
             G :: Set = P (cons x xs)

             rem1 (h1::x <= u) :: G =
                struct fst :: A = x
--                       snd :: least (cons x xs) x = lem4 u x xs u1 h1
                       snd :: least (cons x xs) fst = lem4 u x xs u1 h1

             rem2 (h1::u <= x) :: G =
                struct fst :: A = u
--                       snd :: least (cons x xs) u = lem3 u x xs  u1 h1
                       snd :: least (cons x xs) fst = lem3 u x xs  u1 h1

          in orElim (x<=u) (u<=x) G rem1 rem2 rem

    in seqrec P base indStep
