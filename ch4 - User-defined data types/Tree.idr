{-
data Tree a = Empty
            | Node (Tree a) a (Tree a)
%name Tree tree, tree1, tree2
-}

data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node : Ord elem => (left : BSTree elem) ->
                     (val : elem) -> 
                     (right : BSTree elem) ->
                     BSTree elem

insert : a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x (Node left y right) =
  case compare x y of
       LT => Node (insert x left) y right
       EQ => Node left y right
       GT => Node left y (insert x right)

