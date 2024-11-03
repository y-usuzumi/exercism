module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n > 0 && n <= 64 = Just $ 2 ^ (n - 1)
  | otherwise = Nothing

total :: Integer
total = 2 ^ (64 :: Integer) - 1
