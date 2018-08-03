data Tree a = Empty
            | Node (Tree a) a (Tree a)
%name Tree tree, tree1, tree2

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node left y right) =
  case compare x y of
       LT => Node (insert x left) y right
       EQ => Node left y right
       GT => Node left y (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x $ listToTree xs

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) =
  treeToList left ++ [x] ++ treeToList right

data Exp = Val Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
%name Exp exp1, exp2

total eval : Exp -> Int
eval (Val x) = x
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Sub exp1 exp2) = eval exp1 - eval exp2
eval (Mult exp1 exp2) = eval exp1 * eval exp2

{-
???????????
maxMaybe Nothing Nothing == CantResolve
-}
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe (Just x) (Just y) = Just $ max x y
maxMaybe (Just x) Nothing = Just x
maxMaybe Nothing (Just x) = Just x
maxMaybe Nothing Nothing = Nothing


data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle x h) = 0.5 * x * h
area (Rectangle x y) = x * y
area (Circle r) = pi * r * r

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

total
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive (Triangle x y)) = Just $ area (Triangle x y)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic pic1) = let result_left = biggestTriangle pic
                                         result_right = biggestTriangle pic1 in
                                         maxMaybe result_left result_right
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

