import Data.Vect

total
my_length : List a -> Nat
my_length [] = Z
my_length (x :: xs) = S (my_length xs)

total
my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

total
map : (a -> b) -> List a -> List b
map f [] = []
map f (x :: xs) = f x :: map f xs

total
map2 : (a -> b) -> Vect n a -> Vect n b
map2 f [] = []
map2 f (x :: xs) = f x :: map2 f xs

