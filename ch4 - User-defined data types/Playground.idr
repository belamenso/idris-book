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

rect : Picture
rect = Primitive (Rectangle 20 10)

circ : Picture
circ = Primitive (Circle 5)

tri : Picture
tri = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rect)
                      (Combine (Translate 35 5 circ)
                               (Translate 15 25 tri))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

test : Double
test = pictureArea testPicture


