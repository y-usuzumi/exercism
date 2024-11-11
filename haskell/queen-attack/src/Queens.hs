module Queens (boardString, canAttack) where

import Data.List

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ flip map [0 .. 7] $ \rowIdx ->
  intersperse ' ' $ flip map [0 .. 7] $ \colIdx ->
    case white of
      Just (rowIdxWhite, colIdxWhite)
        | rowIdxWhite == rowIdx && colIdxWhite == colIdx -> 'W'
      _ -> case black of
        Just (rowIdxBlack, colIdxBlack)
          | rowIdxBlack == rowIdx && colIdxBlack == colIdx -> 'B'
        _ -> '_'

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (queenAX, queenAY) (queenBX, queenBY) =
  queenAX == queenBX
    || queenAY == queenBY
    || queenAX + queenAY == queenBX + queenBY
    || queenAX - queenAY == queenBX - queenBY
