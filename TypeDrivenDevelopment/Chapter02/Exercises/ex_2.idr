-- Exercise 1
{-
   (String, String, String)
   List String
   ((Char, String), Char)

-}

-- Exercise 2

palindrome : String -> Bool
palindrome str = str == reverse str

-- Exercise 3

palindrome_q3 : String -> Bool
palindrome_q3 str = let strL = toLower str in
                        strL == reverse strL

-- Exercise 4

palindrome_q4 : String -> Bool
palindrome_q4 str = if length str > 10
                       then palindrome_q3 str
                       else False

-- Exercise 5

palindrome_q5 : Nat -> String -> Bool
palindrome_q5 min str = if length str > min
                           then palindrome_q3 str
                           else False

-- Exercise 6

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

-- Exercise 7

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

-- Exercise 8

over_length : Nat -> List String -> Nat
over_length num xs = let lengths = map length xs in
                         length (filter (> num) lengths)
