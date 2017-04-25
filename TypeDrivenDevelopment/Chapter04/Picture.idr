-- Example of a recursive type.
data MyNat = Z | S MyNat

-- Example of a union type.
||| Represents shapes.
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its lengths and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = ||| Uses the Shape type defined as the primitive.
               Primitive Shape
             | ||| Builds a picture by combining two smaller pictures.
               Combine Picture Picture
             | ||| Builds a picture by rotating another picture through an angle.
               Rotate Double Picture
             | ||| Builds a picture by moving to another location.
               Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic1 pic2) = pictureArea pic1 + pictureArea pic2
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

biggestTriangle : Picture -> Maybe Double
biggestTriangle pic = ?biggestTriangle_rhs
