module errors$warn2a = struct
data T = A | B

f :: T -> T
f A = B
