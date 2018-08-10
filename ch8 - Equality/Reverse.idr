import Data.Vect

myReverse : Vect n a -> Vect n a
myReverse [] = []
myReverse {n = S k} (x :: xs) =
  rewrite plusCommutative 1 k in
          myReverse xs ++ [x]

reverseProof : (x : a) -> (xs : Vect k a) ->
               Vect (k + 1) a -> Vect (S k) a
reverseProof {k} x xs result = rewrite plusCommutative 1 k in result

otherReverse : Vect n a -> Vect n a
otherReverse [] = []
otherReverse (x :: xs) = reverseProof x xs (otherReverse xs ++ [x])

