module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1 = Nothing
  | otherwise = Just $ case sum [v | v <- [1 .. n `div` 2], n `mod` v == 0] `compare` n of
      EQ -> Perfect
      LT -> Deficient
      GT -> Abundant