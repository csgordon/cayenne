module System$Ratio = {
  native Rational :: # = "SUM";
};
module concrete type System$Eq a = {
  (==)		:: a->a->System$Bool.Bool;
  (/=)		:: a->a->System$Bool.Bool;
};
module concrete type System$Ord a = {
  (<=)		:: a->a->System$Bool.Bool;
  (>=)		:: a->a->System$Bool.Bool;
  (<)		:: a->a->System$Bool.Bool;
  (>)		:: a->a->System$Bool.Bool;
  min		:: a->a->a;
  max		:: a->a->a;
  eq		:: System$Eq a;
};
module concrete type System$Show a = {
  showsPrec	:: System$Int.Int -> a -> System$String.String -> System$String.String;
};
module concrete type System$Num a = {
  (+)		:: a->a->a;
  (-)  		:: a->a->a;
  (*)		:: a->a->a;
  negate	:: a->a;
  abs		:: a->a;
  signum	:: a->a;
  fromInt	:: System$Int.Int->a;
  eq		:: System$Eq a;
  show		:: System$Show a;
};
module concrete type System$Real a = {
  toRational	:: System$Ratio.Rational;
  eq		:: System$Eq a;
  ord		:: System$Ord a;
};
module concrete type System$Integral a = {
  quot		:: a->a->a;
  rem		:: a->a->a;
  div		:: a->a->a;
  mod		:: a->a->a;
  toInt		:: a->System$Int.Int;
  real		:: System$Real a;
};
module concrete type System$Monad (m :: # -> #) = {
  (>>=)		:: (a :: #) -> (b :: #) -> m a -> (a -> m b) -> m b;
  (>>)		:: (a :: #) -> (b :: #) -> m a -> m b -> m b;
  return	:: (a :: #) -> a -> m a;
};
