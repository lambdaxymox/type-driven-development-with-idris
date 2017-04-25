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
