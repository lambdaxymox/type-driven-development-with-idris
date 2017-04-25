data Tree elem = ||| A (sub-)tree with no data.
                 Empty
               | ||| A node with a left subtree, a value, and a right subtree.
                 Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1, tree2

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node left val right)
  = case compare x val of
      LT => Node (insert x left) val right
      EQ => Node left val right
      GT => Node left val (insert x right)
