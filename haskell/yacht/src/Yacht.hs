module Yacht (yacht, Category(..)) where

import qualified Data.Array as A
import qualified Data.Foldable as A


data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

countDice :: [Int] -> A.Array Int Int
countDice [] = A.listArray (1, 6) (repeat 0)
countDice (n:ns) = let
  arr = countDice ns
  oldCnt = arr A.! n
  in arr A.// [(n, oldCnt+1)]

yacht :: Category -> [Int] -> Int
yacht c dice = let
  cd = countDice dice
  in case c of
    Ones -> cd A.! 1
    Twos -> 2 * cd A.! 2
    Threes -> 3 * cd A.! 3
    Fours -> 4 * cd A.! 4
    Fives -> 5 * cd A.! 5
    Sixes -> 6 * cd A.! 6
    FullHouse -> if 2 `elem` cd && 3 `elem` cd then sum dice else 0
    FourOfAKind -> sum $ zipWith (\idx cnt -> if cnt >= 4 then idx * 4 else 0) [1..] (A.toList cd)
    LittleStraight -> if cd == A.listArray (1, 6) [1, 1, 1, 1, 1, 0] then 30 else 0
    BigStraight -> if cd == A.listArray (1, 6) [0, 1, 1, 1, 1, 1] then 30 else 0
    Choice -> sum dice
    Yacht -> if 5 `elem` cd then 50 else 0
