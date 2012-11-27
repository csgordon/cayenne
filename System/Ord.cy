module System$Ord = \ (a :: #) -> {
Eq   :: System$Eq a;
(<)  :: a->a->System$Bool.Bool;
(<=) :: a->a->System$Bool.Bool;
(>)  :: a->a->System$Bool.Bool;
(>=) :: a->a->System$Bool.Bool;
max  :: a->a->a;
min  :: a->a->a;
--compare :: a->a->Ordering;
}
