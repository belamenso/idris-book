data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq a => Eq (Tree a) where
  (==) Empty Empty = True
  (==) (Node l1 x r1) (Node l2 y r2) =
    x == y && l1 == l2 && r1 == r2
  (==) _ _ = False

