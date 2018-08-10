occurences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurences item [] = 0
occurences item (x :: xs) = if item == x
                             then 1 + occurences item xs
                             else occurences item xs

data Matter = Solid | Liquid | Gas

Eq Matter where
    (==) Solid Solid = True
    (==) Gas Gas = True
    (==) Liquid Liquid = True
    (==) _ _ = False

