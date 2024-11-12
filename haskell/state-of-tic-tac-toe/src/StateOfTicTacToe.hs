module StateOfTicTacToe (gameState, GameState (..)) where

import Control.Arrow
import Data.List (transpose)

data GameState = Draw | Ongoing | WinX | WinO | Impossible deriving (Eq, Show, Ord)

instance Semigroup GameState where
  a <> b
    | a > b = b <> a
  _ <> Impossible = Impossible
  WinX <> WinO = Impossible
  _ <> WinO = WinO
  _ <> WinX = WinX
  _ <> Ongoing = Ongoing
  _ <> Draw = Draw

instance Monoid GameState where
  mempty = Draw

countPlays :: [String] -> (Int, Int)
countPlays [] = (0, 0)
countPlays plays = foldl (\(xs, os) (xdiff, odiff) -> (xs + xdiff, os + odiff)) (0, 0) (map countRowPlays plays)
  where
    countRowPlays [] = (0, 0)
    countRowPlays (c : cs)
      | c == 'X' = first (1 +) (countRowPlays cs)
      | c == 'O' = second (1 +) (countRowPlays cs)
      | otherwise = countRowPlays cs

gameState :: [String] -> GameState
gameState board
  | (xs, os) <- countPlays board,
    os > xs || xs - os > 1 =
      Impossible
  | otherwise = analyzeBoard
  where
    diagonal1 = [board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2]
    diagonal2 = [board !! 2 !! 0, board !! 1 !! 1, board !! 0 !! 2]
    analyzeCells cells
      | all (== 'X') cells = WinX
      | all (== 'O') cells = WinO
      | ' ' `elem` cells = Ongoing
      | otherwise = Draw
    analyzeBoard = mconcat $ map analyzeCells board ++ map analyzeCells (transpose board) ++ [analyzeCells diagonal1, analyzeCells diagonal2]
