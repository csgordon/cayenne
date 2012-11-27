module example$dimensions =
#include Prelude
open System$Num use * in
open System$Double use Double,read in

abstract
struct

concrete (+%) = System$Num.(+)
concrete (-%) = System$Num.(-)

(+.) = System$Double.(+)
(-.) = System$Double.(-)
(*.) = System$Double.(*)
(/.) = System$Double.(/)

--  The Dimension type must be kept abstract

type Dimension (l :: Num) (t :: Num) = Double

(+) :: (l,t :: Num) |-> Dimension l t -> Dimension l t -> Dimension l t
(+) |l |t x y = x +. y

(-) :: (l,t :: Num) |-> Dimension l t -> Dimension l t -> Dimension l t
(-) |l |t x y = x -. y

(*) :: (l,t,l',t' :: Num) |-> Dimension l t -> Dimension l' t' 
                           -> Dimension (l +% l') (t +% t')
(*) |l |t |l' |t' x y = x *. y

(/) :: (l,t,l',t' :: Num) |-> Dimension l t -> Dimension l' t' 
                           -> Dimension (l -% l') (t -% t')
(/) |l |t |l' |t' x y = x /. y

concrete type Length = Dimension one zero
concrete type Time   = Dimension zero one
concrete type Speed  = Dimension one minusone
concrete type Acceleration  = Dimension one minustwo

length :: Double -> Length
length l = l

speed :: Double -> Speed
speed l = l

time :: Double -> Time
time l = l

{-
Resent-Message-Id: <199902252113.WAA18407@animal.cs.chalmers.se>
Resent-Date:  Thu, 25 Feb 1999 20:24:07 +0000
Resent-From: owner-haskell@dcs.gla.ac.uk
Resent-To: haskell-outgoing@dcs.gla.ac.uk
From: Herbert Graeber <h.graeber@dokom.net>
Sender: owner-haskell@dcs.gla.ac.uk
To: "S.D.Mechveliani" <mechvel@math.botik.ru>
Cc: haskell <haskell@dcs.gla.ac.uk>
Subject: Re: types with value parameter (another example of application)
Date: Thu, 25 Feb 1999 21:01:08 +0100

I have found another useful application of types with value parameters in
C++. I have seen an example of using types to check for proper use of
physical units. I have found this very useful. But I haven't found a way to
express this in haskell. It would be nice to have a type system, that
supports that and similar applications.

Example (In C++, sorry):

    // Here m, l, t anc stand for the exponent of mass, length, time and
current
    template <int m, int l, int t, int c>
    class physical {
        explicit physical(double v);
        ...
    };

    template <int m, int l, int t, int c>
    physical<m, l, t, c> operator + (
        physical<m, l, t, c> v1, physical<m, l, t, c> v2
    ) {
        ...
    }

    template <int m1, int l1, int t1, int c1, int m2, int l2, int t2, int
c2>
    physical<m1+m2, l1+l2, t1+t2, c1+c2> operator * (
        physical<m1, l1, t1, c1> v1, physical<m2, l2, t2, c2> v2
    ) {
        ...
    }

    template <int m1, int l1, int t1, int c1, int m2, int l2, int t2, int
c2>
    physical<m1-m2, l1-l2, t1-t2, c1-c2> operator / (
        physical<m1, l1, t1, c1> v1, physical<m2, l2, t2, c2> v2
    ) {
        ...
    }

    typedef physical<0, 1, 0, 0> Distance;
    typedef physical<0, 0, 1, 0> Time;
    typedef physical<0, 1, 1, 0> Speed;

    Speed s1 = Distance(100.0) / Time(5.0);    // ok
    Speed s1 = s2 + Time(4.0);    // compile time error


-}