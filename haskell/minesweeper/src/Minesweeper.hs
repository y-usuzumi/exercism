module Minesweeper (annotate) where

import Control.Monad
import Control.Monad.ST
import Data.Functor
import GHC.Arr

annotate :: [String] -> [String]
annotate [] = []
annotate board@([] : _) = board
annotate board = runST $ do
  stBoard <- toSTBoard board
  annotateSTBoard stBoard
  displaySTBoard stBoard
  where
    toSTBoard board = do
      let rowBounds = (0, length board - 1)
      let colBounds = (0, length (head board) - 1)
      initialRow <- newSTArray colBounds (0 :: Int)
      arr <- newSTArray rowBounds initialRow
      forM_ [fst rowBounds .. snd rowBounds] $ \rowIdx -> do
        newSTArray colBounds 0 >>= writeSTArray arr rowIdx
      _ <- fillBoard arr board 0
      return arr

    fillBoard arr [] _ = return ()
    fillBoard arr (x : xs) currRowIdx = do
      row <- readSTArray arr currRowIdx
      _ <- fillRow row x 0
      fillBoard arr xs (currRowIdx + 1)

    fillRow row [] _ = return ()
    fillRow row (x : xs) currColIdx = do
      writeSTArray row currColIdx (cellToInt x)
      fillRow row xs (currColIdx + 1)
      where
        cellToInt '*' = -1000
        cellToInt _ = 0

    annotateSTBoard stBoard = do
      let (lowerR, upperR) = boundsSTArray stBoard
      firstRow <- readSTArray stBoard 0
      let (lowerC, upperC) = boundsSTArray firstRow
      forM_ [lowerR .. upperR] $ \rowIdx -> do
        row <- readSTArray stBoard rowIdx
        forM_ [lowerC .. upperC] $ \colIdx -> do
          cell <- readSTArray row colIdx
          when (cell < 0) $ populateMine stBoard (lowerR, upperR) (lowerC, upperC) (rowIdx, colIdx)
      where
        populateMine stBoard (lowerR, upperR) (lowerC, upperC) (mineRowIdx, mineColIdx) = do
          forM_ [mineRowIdx - 1 .. mineRowIdx + 1] $ \rowIdx -> do
            forM_ [mineColIdx - 1 .. mineColIdx + 1] $ \colIdx -> do
              when (rowIdx >= lowerR && rowIdx <= upperR && colIdx >= lowerC && colIdx <= upperC && (rowIdx /= mineRowIdx || colIdx /= mineColIdx)) $ do
                row <- readSTArray stBoard rowIdx
                num <- readSTArray row colIdx
                writeSTArray row colIdx (num + 1)

    displaySTBoard :: STArray s Int (STArray s Int Int) -> ST s [String]
    displaySTBoard stBoard = do
      let (lowerR, upperR) = boundsSTArray stBoard
      forM [lowerR .. upperR] $ \rowIdx -> do
        (readSTArray stBoard rowIdx >>= freezeSTArray) <&> map displayCell . elems
      where
        displayCell 0 = ' '
        displayCell n
          | n > 0 = toEnum (fromEnum '0' + n)
          | otherwise = '*'
