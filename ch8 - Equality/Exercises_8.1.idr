{- 1 -}
total same_cons : {xs : List a} -> {ys : List a} ->
            xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl -- ok, the only possible argument is this one, but
                      -- what about the result?

{- 2 -}
total same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

{- 3 -}
data ThreeEq : a -> b -> c -> Type where
  All3Equal : ThreeEq x x x -- why no "x ->" ???

{- 4 -}
total allSameS : (x, y, z : Nat) -> ThreeEq x y z ->
           ThreeEq (S x) (S y) (S z)
allSameS z z z All3Equal = All3Equal

