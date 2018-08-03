import Data.Vect

Matrix : Type -> Nat -> Nat -> Type
Matrix x k j = Vect k (Vect j x)

total careateEmpties : Matrix a m 0
careateEmpties {m} = replicate m []

total transposeMat : Matrix a n m -> Matrix a m n
transposeMat [] = careateEmpties
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)

total addMatrix : Num a => Matrix a n m -> Matrix a n m -> Matrix a n m
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

total multiplyMat : Num a => Matrix a n m -> Matrix a m k -> Matrix a n k
multiplyMat xs ys =
  let cols = transposeMat ys in
  map (\old_row => map (\col => sum $ zipWith (*) old_row col) cols) xs

test : Matrix Int 2 3
test = transposeMat [[1, 2],[3, 4], [5, 6]]

test2 : Matrix Int 2 1
test2 = addMatrix [[1], [1]] [[7], [10]]

test3 : Matrix Int 3 4
test3 = multiplyMat [[1,2], [3,4], [5,6]] [[7,8,9,10], [11,12,13,14]]

