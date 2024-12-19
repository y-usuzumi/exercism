module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.Function (on)
import Data.List

isPalindrome :: Integer -> Bool
isPalindrome v =
  let s = show v
   in s == reverse s

palindromes :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromes minFactor maxFactor = sortOn fst $ filter (isPalindrome . fst) [(x * y, (x, y)) | x <- [minFactor .. maxFactor], y <- [x .. maxFactor]]

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  let groupedPals = reverse $ groupBy ((==) `on` fst) (palindromes minFactor maxFactor)
   in case groupedPals of
        [] -> Nothing
        (x : _) -> Just (fst (head x), map snd x)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  let groupedPals = groupBy ((==) `on` fst) (palindromes minFactor maxFactor)
   in case groupedPals of
        [] -> Nothing
        (x : _) -> Just (fst (head x), map snd x)
