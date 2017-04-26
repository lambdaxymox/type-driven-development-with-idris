import Data.Vect

||| A type synonym for describing positions as (x,y) coordinates.
Position : Type
Position = (Double, Double)

||| A type synonym for describing polygons with n corners.
Polygon : Nat -> Type
Polygon n = Vect n Position

tri : Polygon 3
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]
