module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree (..))
import Data.List

splitOn :: (Ord a, Eq a) => a -> [a] -> Maybe ([a], [a])
splitOn _ [] = Nothing
splitOn a (x : xs)
  | a == x = Just ([], xs)
  | otherwise = do
      (l, r) <- splitOn a xs
      return (x : l, r)

treeFromTraversals :: (Ord a) => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Nothing
treeFromTraversals a b
  | nub a /= a || nub b /= b = Nothing -- Darn O(n^2)
  | otherwise = _treeFromTraversals a b

_treeFromTraversals :: (Ord a) => [a] -> [a] -> Maybe (BinaryTree a)
_treeFromTraversals [] [] = Just Leaf
_treeFromTraversals [] _ = Nothing
_treeFromTraversals _ [] = Nothing
_treeFromTraversals (a : as) bs = do
  (l, r) <- splitOn a bs
  let (asl, asr) = splitAt (length l) as
  brl <- _treeFromTraversals asl l
  brr <- _treeFromTraversals asr r
  return $ Branch brl a brr