data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
              Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right)
    = case compare x val of
           LT => Node (insert x left) val right
           EQ => orig
           GT => Node left val (insert x right)

-- Exercise 1
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

-- Exercise 2
-- The function `treeToList` does an in-order traversal of a tree.
treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = (treeToList left) ++ x :: treeToList right

-- Exercise 3
data Expr = Val Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
-- Exercise 4
evaluate : Expr -> Integer
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mul x y) = evaluate x * evaluate y

-- Exercise 5
maybeMax : Ord a => Maybe a -> Maybe a -> Maybe a
maybeMax Nothing Nothing = Nothing
maybeMax Nothing (Just x) = Just x
maybeMax (Just x) Nothing = Just x
maybeMax (Just x) (Just y) = Just $ max x y

-- Exercise 6
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive tri@(Triangle x y)) = Just $ area tri
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic1 pic2) = maybeMax (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
