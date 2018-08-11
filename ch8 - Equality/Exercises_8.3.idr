data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a


{- 1 -} -- ???
total headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
       (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

total tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
       (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

{- 2 -}
total DecEq a => DecEq (Vect n a) where
	-- (x1 : t) -> (x2 : t) -> Dec (x1 = x2)
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) =
    case decEq x y of
         No contra => No (headUnequal contra)
         Yes Refl => case decEq xs ys of
                          No contra => No (tailUnequal contra)
                          Yes Refl => Yes Refl

