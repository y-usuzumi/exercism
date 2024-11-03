module SumOfMultiples (sumOfMultiples) where

import Control.Monad
import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub . join . map (values limit) $ factors
  where
    values n 0
      | n > 0 = [0]
      | otherwise = []
    values limit factor = takeWhile (< limit) [factor, 2 * factor ..]