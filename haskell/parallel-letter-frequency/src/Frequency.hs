module Frequency (frequency) where

import qualified Data.List as L
import qualified Control.Parallel.Strategies as P
import Data.Map  (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

splitEvenlyTo :: Int -> [a] -> [[a]]
splitEvenlyTo n [] = replicate n []
splitEvenlyTo n texts = let
  (piece, remaining) = splitAt n texts
  remainingResult = splitEvenlyTo n remaining
  in zipWithTail (:) piece remainingResult
  where
    zipWithTail _ [] b = b
    zipWithTail _ _ [] = []
    zipWithTail f (a:as) (b:bs) = f a b: zipWithTail f as bs

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = let
  chunks = splitEvenlyTo nWorkers texts
  in L.foldl' (M.unionWith (+)) M.empty (map process chunks `P.using` P.parList P.rdeepseq)
  where
    process = L.foldl' (M.unionWith (+)) M.empty . map countLetters
    countLetters = T.foldl' (flip $ M.alter (Just . maybe 1 (+1))) M.empty . T.filter (not . (`elem` " ,.?!-0123456789")) . T.toLower
