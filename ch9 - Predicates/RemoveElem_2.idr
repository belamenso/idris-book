import Data.Vect

maryInVector : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVector = There (There Here)

total removeElem : (value : a) ->
             (xs : Vect (S n) a) ->
             (prf : Elem value xs) -> -- this line means that we can assume
                                      -- that value is in xs, and that the
                                      -- caller has provide the proof
             Vect n a
removeElem value (value :: ys) Here = ys
removeElem value {n = Z} (y :: []) (There later) = absurd later
-- ^ this line is unnecessary
removeElem value {n = (S k)} (y :: ys) (There later) =
  y :: removeElem value ys later

total removeElem_auto : (value : a) -> (xs : Vect (S n) a) ->
                  {auto prf : Elem value xs} -> Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf

removeElement : (value : a) -> (xs : Vect (S n) a) ->
                {auto prf : Elem value xs} -> Vect n a
removeElement value (value :: ys) {prf = Here} = ys
removeElement {n = Z} value (y :: []) {prf = There later} = absurd later
removeElement {n = S k} value (y :: ys) {prf = There later} =
  y :: removeElement value ys

