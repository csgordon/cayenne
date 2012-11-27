module example$misc = concrete struct
Set = #

Type = #1

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

--data (+) (A::Set) (B::Set) = Inl (x::A) | Inr (y::B)
data (+) (A::Set) (B::Set) = Inl A | Inr B

orElim (A::Set) (B::Set) (C::Set) (h1::A->C) (h2::B->C) (h::A+B) :: C =
 case h of
   (Inl x) -> h1 x
   (Inr y) -> h2 y

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

