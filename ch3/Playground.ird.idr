module Main

import Data.Vect

invert : Bool -> Bool
invert False = True
invert True = False

describeList : List Int -> String
describeList [] = "Empty"
describeList (x :: xs) = "Not empty, tail = " ++ show xs

allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

f : Nat -> Bool
f Z = ?f_rhs_1
f (S k) = ?f_rhs_2

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

fourInts : Vect 4 Int
fourInts = ?fourInts_rhs

createEmpties : Vect n (Vect 0 elem)
createEmpties {n} = replicate n []

transposeHelper : (x : Vect n elem) ->
                  (xsTrans : Vect n (Vect len elem)) ->
                  Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         transposeHelper x xsTrans

