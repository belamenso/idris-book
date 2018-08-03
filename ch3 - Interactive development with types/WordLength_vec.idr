import Data.Vect

total
allLenghts : Vect len String -> Vect len Nat
allLenghts [] = []
allLenghts (x :: xs) = length x :: allLenghts xs

