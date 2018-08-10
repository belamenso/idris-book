data Tree a = Empty
            | Node (Tree a) a (Tree a)

Foldable Tree where
  foldr f acc Empty = acc
  foldr f acc (Node left x right) =
    let l = foldr f acc left
        r = foldr f l right in
        f x r

