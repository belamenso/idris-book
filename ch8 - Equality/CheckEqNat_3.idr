
zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible

sucNotZero : (S k = 0) -> Void
sucNotZero Refl impossible

{- Given a proof that two numbers aren't equal, and a proof that
their successors are equal, produce a value of the empty type -}
notRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
notRec contra Refl = contra Refl -- ???

checkEqNat : (n1 : Nat) -> (n2 : Nat) -> Dec (n1 = n2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSuc
checkEqNat (S k) Z = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Yes prf => Yes (cong prf)
                              No contra => No (notRec contra)

