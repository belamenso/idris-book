
{- 1 -} -- ???

data Elem : a -> List a -> Type where
  Here : Elem x (x :: xs)
  There : Elem x xs -> Elem x (y :: xs)

{- 2 -}
data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

notLastInNil : Last [] value -> Void
notLastInNil LastOne impossible
notLastInNil (LastCons _) impossible

notTheLast : (contra : (x = value) -> Void) -> Last [x] value -> Void
notTheLast contra LastOne = contra Refl
notTheLast _ (LastCons LastOne) impossible
notTheLast _ (LastCons (LastCons _)) impossible

lastNotCons : (contra : Last (y :: xs) value -> Void) ->
              Last (x :: (y :: xs)) value -> Void
lastNotCons contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notLastInNil
isLast (x :: []) value = case decEq x value of
                              Yes Refl => Yes LastOne
                              No contra => No (notTheLast contra)
isLast (x :: y :: xs) value = case isLast (y :: xs) value of
                                   Yes prf => Yes (LastCons prf)
                                   No contra => No (lastNotCons contra)

