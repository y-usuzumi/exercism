module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz n
  | n > 0 = (+1) <$> let (q, r) = n `quotRem` 2 in if r == 0 then (collatz q) else collatz (3 * n + 1)
  | otherwise = Nothing
