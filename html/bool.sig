System$Bool ::
sig {
  data Bool = False | True;
  if :: (a :: #) |-> Bool -> a -> a -> a;
  (&&) :: Bool -> Bool -> Bool;
  (||) :: Bool -> Bool -> Bool;
  not :: Bool -> Bool;
  show :: Bool -> String;
};
