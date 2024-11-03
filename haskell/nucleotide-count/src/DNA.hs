module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = _nucleotideCounts xs $ M.fromList [ (A, 0)
                                                        , (C, 0)
                                                        , (G, 0)
                                                        , (T, 0)
                                                        ]
  where
    _nucleotideCounts [] m = Right m
    _nucleotideCounts (x : xs) m
      | x == 'A' = _nucleotideCounts xs (adjust (+ 1) A m)
      | x == 'C' = _nucleotideCounts xs (adjust (+ 1) C m)
      | x == 'G' = _nucleotideCounts xs (adjust (+ 1) G m)
      | x == 'T' = _nucleotideCounts xs (adjust (+ 1) T m)
      | otherwise = Left "error"
