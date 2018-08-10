data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y 
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs $ eval x

Num a => Num (Expr a) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger -- ?

Neg a => Neg (Expr a) where
  negate x = 0 - x
  (-) = Sub

Abs a => Abs (Expr a) where
  abs = Abs

Show a => Show (Expr a) where
  show (Val x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) =  "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) =  "(" ++ show x ++ " `div` " ++ show y ++ ")"
  show (Abs x) =  "|" ++ show x ++ "|"

(Eq a, Neg a, Abs a, Integral a) => Eq (Expr a) where
  (==) x y = eval x == eval y

(Neg a, Abs a, Integral a) => Cast (Expr a) a where
  cast x = eval x

Functor Expr where
  map f (Val x) = Val $ f x
  map f (Add x y) = Add (map f x) (map f y)
  map f (Sub x y) = Sub (map f x) (map f y)
  map f (Mul x y) = Mul (map f x) (map f y)
  map f (Div x y) = Div (map f x) (map f y)
  map f (Abs x) = Abs (map f x)

test : Expr Integer
test = 1 + 3 * 7 - 10

test2 : Integer
test2 = eval test

