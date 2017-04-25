import Data.Vect

-- Exercise 1
createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

{-
transposeHelper : (x : Vect n elem)
               -> (xTrans : Vect n (Vect k elem))
               -> Vect n (Vect (S k) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys
-}

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xTrans = transposeMat xs in
                             zipWith (::) x xTrans

-- Exercise 2
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

addMatrix' : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix' mat1 mat2 = zipWith (zipWith (+)) mat1 mat2

(+) : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
(+) = addMatrix

-- Exercise 3
