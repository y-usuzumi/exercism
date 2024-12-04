module ProteinTranslation (proteins) where

import Data.List.Split

mapProtein :: String -> String
mapProtein p
  | p == "AUG" = "Methionine"
  | p == "UUU" || p == "UUC" = "Phenylalanine"
  | p == "UUA" || p == "UUG" = "Leucine"
  | p `elem` ["UCU", "UCC", "UCA", "UCG"] = "Serine"
  | p == "UAU" || p == "UAC" = "Tyrosine"
  | p == "UGU" || p == "UGC" = "Cysteine"
  | p == "UGG" = "Tryptophan"
  | p `elem` ["UAA", "UAG", "UGA"] = "STOP"
  | otherwise = "ERROR"

proteins :: String -> Maybe [String]
proteins s =
  let pl = foldr ((:) . mapProtein) [] (chunksOf 3 s)
      (pl', prem) = break (\p -> p == "STOP" || p == "ERROR") pl
   in case prem of
        [] -> Just pl'
        ("STOP" : _) -> Just pl'
        _ -> Nothing
