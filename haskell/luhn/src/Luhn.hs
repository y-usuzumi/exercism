module Luhn (isValid) where

normalize :: String -> Maybe String
normalize [] = Just []
normalize (x : xs)
  | x == ' ' = normalize xs
  | x `elem` ['0' .. '9'] = (x :) <$> normalize xs
  | otherwise = Nothing

isValid :: String -> Bool
isValid n
  | (Just s) <- normalize n,
    luhn s =
      True
  | otherwise = False
  where
    luhn [] = False
    luhn [_] = False
    luhn s = sum (zipWith ($) transformers (reverse (map (read . (: [])) s :: [Int]))) `mod` 10 == 0
    transformers = id : (\d -> if d > 4 then d * 2 - 9 else d * 2) : transformers
