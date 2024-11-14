module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance [] _ = Nothing
distance _ [] = Nothing
distance (a : as) (b : bs)
  | a == b = distance as bs
  | otherwise = (1 +) <$> distance as bs
