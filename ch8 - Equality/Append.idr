data Vect : Nat -> Type -> Type where
  Nil : Vect 0 a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

append_nil : Vect m a -> Vect (plus m 0) a
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs
-- append_nil {m} xs = rewrite plusCommutative m 0 in xs

append_xs : Vect (S (m + k)) a -> Vect (plus m (S k)) a
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs

append : Vect n a -> Vect m a -> Vect (m + n) a
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)

