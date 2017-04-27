import Data.Vect


myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = let rev_xs = myReverse xs
                          result = rev_xs ++ [x]
                       in ?myReverse_rhs_2
