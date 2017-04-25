import Data.Vect

myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

myReverse' : (acc : List a) -> (xs : List a) -> List a
myReverse' acc [] = acc
myReverse' acc (x :: xs) = let acc' = x :: acc in myReverse' acc' xs

myReverse : List a -> List a
myReverse xs = myReverse' [] xs


myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

myMapVect : (a -> b) -> Vect n a -> Vect n b
myMapVect f [] = []
myMapVect f (x :: xs) = f x :: myMapVect f xs
