checkEqNat : (n1 : Nat) -> (n2 : Nat) -> Maybe (n1 = n2)
checkEqNat Z Z = Just Refl
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just prf => Just $ cong prf
checkEqNat _ _ = Nothing

