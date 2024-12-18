module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n = _primesUpTo n [2 .. n] []
  where
    _primesUpTo _ [] _ = []
    _primesUpTo n (x : xs) marked
      | x `elem` marked = _primesUpTo n xs marked
      | otherwise = x : _primesUpTo n xs (marked ++ [x, 2 * x .. n])
