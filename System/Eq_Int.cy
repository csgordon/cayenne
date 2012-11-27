module System$Eq_Int :: System$Eq Int = {
    native (==) :: Int->Int->System$Bool.Bool = "\\x.\\y.x==(y::Int)";
    native (/=) :: Int->Int->System$Bool.Bool = "\\x.\\y.x/=(y::Int)";
}
