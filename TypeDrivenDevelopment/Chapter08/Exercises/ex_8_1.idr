-- Exercise 1
total
same_cons :  {xs : List a}
          -> {ys : List a}
          -> xs = ys
          -> x :: xs = x :: ys
same_cons Refl = Refl

-- Exercise 2
same_list :  {xs : List a}
          -> {ys : List a}
          -> x = y
          -> xs = ys
          -> x :: xs = y :: ys
same_list Refl Refl = Refl

-- Exercise 3
data ThreeEq : a -> b -> c -> Type where
  AllSame : ThreeEq x x x

-- Exercise 4
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x AllSame = AllSame
