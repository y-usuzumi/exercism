module Acronym (abbreviate) where

import Control.Monad
import Data.Char

splitWords :: String -> [String]
splitWords s = reverse . map reverse $ _splitWords [""] s
  where
    _splitWords ws "" = ws
    _splitWords [] _ = error "Impossible"
    _splitWords (w : ws) (x : xs)
      | x == ' ' || x == '-' = _splitWords ("" : w : ws) xs
      | otherwise = case w of
          (c1 : _)
            | lowerAndUpper c1 x -> _splitWords ("" : w : ws) (x : xs)
          _ -> _splitWords ((x : w) : ws) xs
    lowerAndUpper c1 c2 = c1 `elem` ['a' .. 'z'] && c2 `elem` ['A' .. 'Z']

takeFirstNonSymbol :: String -> String
takeFirstNonSymbol [] = []
takeFirstNonSymbol (x : xs)
  | x `elem` ['A' .. 'Z'] || x `elem` ['a' .. 'z'] = x : ""
  | otherwise = takeFirstNonSymbol xs

abbreviate :: String -> String
abbreviate xs = join $ map (map toUpper . takeFirstNonSymbol) $ splitWords xs
