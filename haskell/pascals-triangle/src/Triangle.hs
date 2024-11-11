module Triangle (rows) where

reverseRows :: Int -> [[Integer]]
reverseRows 0 = []
reverseRows 1 = [[1]]
reverseRows n =
  let prevRows = reverseRows (n - 1)
      newRow = zipWith (+) (0 : head prevRows) (head prevRows ++ [0])
   in newRow : prevRows

rows :: Int -> [[Integer]]
rows = reverse . reverseRows
