module Triangle (TriangleType (..), triangleType) where

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | a <= 0 || b <= 0 || c <= 0 = Illegal
  | a > b = triangleType b a c
  | b > c = triangleType a c b
  | a == c = Equilateral
  | a + b < c = Illegal
  | a == b || b == c = Isosceles
  | otherwise = Scalene
