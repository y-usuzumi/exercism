{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module WordSearch (search, CharPos (..), WordPos (..)) where

import Control.Arrow
import Control.Monad
import qualified Data.Array as A
import qualified Data.Foldable as A
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe
import GHC.IO (unsafePerformIO)

type Arr = A.Array Int Char

type Matrix = A.Array Int Arr

data CharPos = CharPos {col :: Int, row :: Int} deriving (Eq, Show)

data WordPos = WordPos {start :: CharPos, end :: CharPos} deriving (Eq, Show)

data Direction = North | West | South | East deriving (Eq, Show)

allDirections :: [(Int, Int)]
allDirections =
  [ (-1, 0),
    (-1, -1),
    (0, -1),
    (1, -1),
    (1, 0),
    (1, 1),
    (0, 1),
    (-1, 1)
  ]

listToArr :: String -> A.Array Int Char
listToArr arr = A.array (1, length arr) $ zip [1 ..] arr

gridToMatrix :: [String] -> Matrix
gridToMatrix grid = A.array (1, length grid) $ zip [1 ..] $ map listToArr grid

collectStartingPoints :: Matrix -> M.Map Char [CharPos]
collectStartingPoints matrix =
  let rows = A.assocs matrix
      coordCharTuples = join . flip map rows $ \(idxR, r) -> map (first (idxR,)) (A.assocs r)
   in foldl' (\m ((r, c), ch) -> M.alter (Just . (CharPos {col = c, row = r} :) . fromMaybe []) ch m) M.empty coordCharTuples

findWord :: Matrix -> M.Map Char [CharPos] -> String -> [WordPos]
findWord _ _ [] = []
findWord matrix startingPoints word@(ch : _)
  | Just sps <- M.lookup ch startingPoints =
      join $ map (\sp -> findWordFromStartingPoint sp sp) sps
  | otherwise = []
  where
    findWordFromStartingPoint sp (CharPos {col, row}) =
      mapMaybe (\d -> fwfspwd sp (CharPos {col, row}) d word) allDirections
    fwfspwd _ _ _ [] = error "Impossible"
    fwfspwd sp curr@(CharPos {col, row}) (offsetR, offsetC) [ch]
      | row > 0
          && row <= A.length matrix
          && col > 0
          && col <= A.length (matrix A.! 1)
          && matrix A.! row A.! col
            == ch =
          Just $ WordPos {start = sp, end = curr}
      | otherwise = Nothing
    fwfspwd sp (CharPos {col, row}) (offsetR, offsetC) (ch : chs)
      | row > 0
          && row <= A.length matrix
          && col > 0
          && col <= A.length (matrix A.! 1)
          && matrix A.! row A.! col
            == ch =
          fwfspwd sp CharPos {col = col + offsetC, row = row + offsetR} (offsetR, offsetC) chs
      | otherwise = Nothing

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList =
  let matrix = gridToMatrix grid
      startingPoints = collectStartingPoints matrix
   in map (\w -> (w, listToMaybe (findWord matrix startingPoints w))) wordList