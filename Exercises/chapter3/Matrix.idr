import Data.Vect


createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem)
               -> (xTrans : Vect n (Vect k elem))
               -> Vect n (Vect (S k) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xTrans = transposeMat xs in
                             transposeHelper x xTrans
