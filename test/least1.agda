(&) (A::Set) (B::Set) :: Set =  
 sig 
  fst::A
  snd::B

sigma (A::Set) (B::A -> Set) :: Set =  
 sig 
  fst::A
  snd::B fst

pair (A::Set)(B::Set) (a::A) (b::B) :: A & B = 
  struct
   fst::A=a
   snd::B=b

(<->) (A::Set) (B::Set) :: Set = (A->B) & (B->A)

data (+) (A::Set) (B::Set) = Inl (x::A) | Inr (y::B)

orElim (A::Set) (B::Set) (C::Set) (h1::A->C) (h2::B->C) (h::A+B) :: C =
 case h of
   (Inl x) -> h1 x
   (Inr y) -> h2 y


package sequence (A::Set) =
 struct

 abstract Seq :: Set = ?
 abstract singl :: A -> Seq = ?
 abstract cons :: A -> Seq -> Seq = ?
 abstract seqrec :: (P::Seq -> Set) -> 
                    ((x::A) -> P (singl x)) ->
                    ((x::A) -> (xs::Seq) -> P xs -> P (cons x xs)) ->
                    (xs::Seq) -> P xs
  = ?
  
 abstract forall :: (A -> Set) -> Seq -> Set = ?
 abstract fa1 :: (p::A->Set) ->  (x::A) -> ((forall p (singl x)) <-> p x)
  = ?
  
 abstract fa2 :: (p::A->Set) -> (x::A) -> (xs::Seq) ->
                 ((forall p (cons x xs)) <-> (p x & forall p xs))
  = ? 
 abstract exists :: (A -> Set) -> Seq -> Set = ?
 abstract ex1 :: (p::A->Set) ->  (x::A) -> ((exists p (singl x)) <-> p x)
  = ?
 abstract ex2 :: (p::A->Set) -> (x::A) -> (xs::Seq) ->
                 ((exists p (cons x xs)) <-> (p x + exists p xs))
  = ? 
  
 lem1 (p::A->Set) (q::A->Set) (h::(x::A) -> p x -> q x) 
      ::  (xs::Seq) -> (forall p xs) -> forall q xs =

   let P (xs::Seq):: Set = forall p xs -> forall q xs
       base (x::A) :: P (singl x) =
            \(h' :: forall p (singl x)) ->
             (fa1 q x).snd (h x ((fa1 p x).fst h')) 
       indStep (x::A) (xs::Seq) (h1 :: P xs) :: P (cons x xs) =
             \(h' :: forall p (cons x xs)) ->
             let rem1 :: (p x) & (forall p xs) = (fa2 p x xs).fst  h'
                 rem2 :: p x = rem1.fst
                 rem3 :: q x = h x rem2
                 rem4 :: forall p xs = rem1.snd
                 rem5 :: forall q xs = h1 rem4
             in  (fa2 q x xs).snd (pair (q x) (forall q xs) rem3 rem5) 

   in seqrec P base indStep

 lem3 (p::A->Set) (x::A) (xs::Seq) (h::exists p xs) 
               :: exists p (cons x xs) =
   (ex2 p x xs).snd (Inr@((p x) + (exists p xs)) h)

 lem4 (p::A->Set) (x::A) (xs::Seq) (h::p x) 
               :: exists p (cons x xs) =
   (ex2 p x xs).snd (Inl@((p x) + (exists p xs)) h)

rel(A::Set)::Type = A->A->Set
pred(A::Set)::Type = A -> Set

reflexive (A::Set) (R :: rel A) :: Set = (a::A) -> R a a
symmetric (A::Set) (R :: rel A) :: Set =
 (a1::A) -> (a2::A) -> (u::R a1 a2)  -> R a2 a1
transitive (A::Set) (R :: rel A) :: Set =
 (a1::A) -> (a2::A) -> (a3::A) ->  (u1::R a1 a2) ->  (u2::R a2 a3) ->  R a1 a3

poset (U :: Set) ((<=) :: rel U) :: Set = 
 reflexive U (<=) & transitive U (<=)

linear (U::Set)  ((<=) :: rel U) :: Set = 
 (x::U) -> (y::U) -> (x <= y) + (y <= x)


package least (A::Set) ((<=) :: rel A) (p::poset A (<=)) (l::linear A (<=)) =

 open sequence A use Seq, forall, fa1, fa2, exists, ex1, ex2, singl,
                     cons, seqrec
 in 


 struct

 (==) :: rel A = \ (x::A) -> \ (y::A) -> (x<=y) & (y<=x)

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

 problem :: (xs :: Seq) -> sigma A (least xs) =

    let

     P (xs::Seq) :: Set = sigma A (least xs)

     base (x::A) :: P (singl x) =
         struct
            fst :: A = x
            snd :: least (singl x) x = lem5 x

     indStep (x::A) (xs::Seq) (h::P xs) :: P (cons x xs) =
         let u :: A = h.fst
             u1 :: least xs u = h.snd
             rem :: (x <= u) + (u <= x) = l x u
             G :: Set = P (cons x xs)

             rem1 (h1::x <= u) :: G =
                struct fst :: A = x
                       snd :: least (cons x xs) x = lem4 u x xs u1 h1

             rem2 (h1::u <= x) :: G =
                struct fst :: A = u
                       snd :: least (cons x xs) u = lem3 u x xs  u1 h1

          in orElim (x<=u) (u<=x) G rem1 rem2 rem

    in seqrec P base indStep