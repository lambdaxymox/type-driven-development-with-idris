totalLen : List String -> Nat
totalLen xs = foldr (\str, len => len + length str) 0 xs
