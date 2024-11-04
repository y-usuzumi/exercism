module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just $ sieves [2, 3 ..] !! (n - 1)
  where
    sieves [] = error "Impossible"
    sieves (x : xs) = x : sieves [v | v <- xs, v `mod` x > 0]
