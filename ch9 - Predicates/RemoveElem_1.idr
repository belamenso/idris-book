import Data.Vect

removeElem : DecEq a => (value : a) -> (xs : Vect (S n) a) -> Vect n a
removeElem value (x :: xs) = case decEq value x of
                                  Yes Refl => xs
                                  No contra => x :: removeElem value xs
{- the problem is, xs argument to removeElem needs to be non-empty,
   and xs in the last line can be empty -}

{- you can do 3 things:
  * return Maybe (Vect n a), Nothing is the value doesn't appear
  * return a dependent pair (newLength ** Vect newLength a)
  * express a precondition on removeElem that the value is there -}

