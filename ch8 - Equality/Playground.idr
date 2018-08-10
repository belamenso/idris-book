data EqNat : (n1 : Nat) -> (n2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

-- "You can see from the type of the lifted definition, sameS, that it’s a function that takes evidence that k and j are equal, and returns evidence that S k and S j are equal."
sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) ->
        EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

checkEqNat : (n1 : Nat) -> (n2 : Nat) -> Maybe (EqNat n1 n2)
checkEqNat Z Z = Just $ Same 0
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just eq => Just (sameS k j eq)
checkEqNat _ _ = Nothing

