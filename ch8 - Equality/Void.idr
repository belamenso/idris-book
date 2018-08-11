
total proofIneq : 2 + 2 = 5 -> Void
proofIneq Refl impossible

t : Dec (4 = 5)
t = No proofIneq

total valueNotSuc : (x : Nat) -> x = S x -> Void
valueNotSuc _ Refl impossible

