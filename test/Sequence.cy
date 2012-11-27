module concrete example$Sequence = 
open example$misc use * in
(A::Set) ->
sig

 Seq :: Set
 singl :: A -> Seq
 cons :: A -> Seq -> Seq
 seqrec :: (P::Seq -> Set) -> 
                    ((x::A) -> P (singl x)) ->
                    ((x::A) -> (xs::Seq) -> P xs -> P (cons x xs)) ->
                    (xs::Seq) -> P xs
  
 forall :: (A -> Set) -> Seq -> Set
 fa1 :: (p::A->Set) ->  (x::A) -> ((forall p (singl x)) <-> p x)
  
 fa2 :: (p::A->Set) -> (x::A) -> (xs::Seq) ->
                 ((forall p (cons x xs)) <-> (p x & forall p xs))

 exists :: (A -> Set) -> Seq -> Set
 ex1 :: (p::A->Set) ->  (x::A) -> ((exists p (singl x)) <-> p x)
 ex2 :: (p::A->Set) -> (x::A) -> (xs::Seq) ->
                 ((exists p (cons x xs)) <-> (p x + exists p xs))
  
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
