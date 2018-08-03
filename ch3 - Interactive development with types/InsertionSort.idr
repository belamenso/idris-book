import Data.Vect

total insert : Ord elem => elem -> Vect len elem -> Vect (S len) elem
insert x [] = [x]
insert x (y :: ys) = if x <= y then x :: y :: ys
                               else y :: insert x ys

total insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = insert x (insSort xs)


