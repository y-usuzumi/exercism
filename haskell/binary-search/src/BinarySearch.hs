module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = let
  (lb, ub) = bounds array
  in _find array lb ub x
  where
    _find array lb ub x
      | lb > ub = Nothing
      | otherwise = let
        mid = (lb + ub) `div` 2
        in case (array ! mid) `compare` x of
          LT -> _find array (mid+1) ub x
          GT -> _find array lb (mid-1) x
          EQ -> Just mid
