import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

triangle : Polygon 3
triangle = [(?triangle_rhs1, ?triangle_rhs2),
            (?triangle_rhs3, ?triangle_rhs4),
            (?triangle_rhs5, ?triangle_rhs6)]

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "hello"
getStringOrInt True = 101

