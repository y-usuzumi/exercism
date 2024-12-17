module Transpose (transpose) where
import Data.List hiding (transpose)
import Data.Char
import Data.Maybe (isNothing, fromMaybe)

transpose :: [String] -> [String]
transpose lines
  | all null lines = []
  | otherwise = let
    headTails = map uncons lines
    heads = map (fromMaybe ' ') $ reverse . dropWhile isNothing . reverse $ map (fst <$>) headTails
    tails = map (maybe [] snd) headTails
    in heads:transpose tails
