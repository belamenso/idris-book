import Data.Fin
import Data.Vect

{-
data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a
%name Vect xs, ys, zs
-}

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: Main.zip xs ys


tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} idx xs = case integerToFin idx n of
                      Nothing => Nothing
                      Just x => Just $ index x xs

