data Vect : (len : Nat) -> (elem : Type) -> Type where
  Nil : Vect Z elem
  (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

Eq a => Eq (Vect n a) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys
  (==) _ _ = False

Foldable (Vect n) where
  foldr f acc [] = acc
  foldr f acc (x :: xs) = f x (foldr f acc xs)

t1 : Vect 3 Int
t1 = [1,2,3]

t2 : Vect 3 Int
t2 = [1,2,3]

t3 : Int
t3 = foldr (+) 0 t2

