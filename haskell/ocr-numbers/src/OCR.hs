module OCR (convert) where

import Control.Monad
import Data.List

convertLines :: [String] -> [String]
convertLines [] = []
convertLines [x1 : x2 : x3 : xs, y1 : y2 : y3 : ys, z1 : z2 : z3 : zs, w1 : w2 : w3 : ws] =
  [processChar x1 x2 x3 y1 y2 y3 z1 z2 z3 w1 w2 w3 : head (convertLines [xs, ys, zs, ws])]
convertLines [_, _, _, _] = [""]
convertLines n
  | length n >= 4 = convertLines (take 4 n) ++ convertLines (drop 4 n)
  | otherwise = error "Impossible"

{- ORMOLU_DISABLE -}
processChar :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char
processChar ' ' '_' ' '
            '|' ' ' '|'
            '|' '_' '|'
            ' ' ' ' ' ' = '0'
processChar ' ' ' ' ' '
            ' ' ' ' '|'
            ' ' ' ' '|'
            ' ' ' ' ' ' = '1'
processChar ' ' '_' ' '
            ' ' '_' '|'
            '|' '_' ' '
            ' ' ' ' ' ' = '2'
processChar ' ' '_' ' '
            ' ' '_' '|'
            ' ' '_' '|'
            ' ' ' ' ' ' = '3'
processChar ' ' ' ' ' '
            '|' '_' '|'
            ' ' ' ' '|'
            ' ' ' ' ' ' = '4'
processChar ' ' '_' ' '
            '|' '_' ' '
            ' ' '_' '|'
            ' ' ' ' ' ' = '5'
processChar ' ' '_' ' '
            '|' '_' ' '
            '|' '_' '|'
            ' ' ' ' ' ' = '6'
processChar ' ' '_' ' '
            ' ' ' ' '|'
            ' ' ' ' '|'
            ' ' ' ' ' ' = '7'
processChar ' ' '_' ' '
            '|' '_' '|'
            '|' '_' '|'
            ' ' ' ' ' ' = '8'
processChar ' ' '_' ' '
            '|' '_' '|'
            ' ' '_' '|'
            ' ' ' ' ' ' = '9'
processChar _ _ _ _ _ _ _ _ _ _ _ _ = '?'
{- ORMOLU_ENABLE -}

convert :: String -> String
convert xs = intercalate "," $ convertLines (lines xs)
