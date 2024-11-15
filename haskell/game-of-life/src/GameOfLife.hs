{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameOfLife (tick) where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.STRef

type STGrid s = STArray s Int (STArray s Int Int)

neighbors :: [(Int, Int)]
neighbors =
  [ (0, -1),
    (1, -1),
    (1, 0),
    (1, 1),
    (0, 1),
    (-1, 1),
    (-1, 0),
    (-1, -1)
  ]

emptyST :: Int -> Int -> ST s (STGrid s)
emptyST rows cols = newGenArray (0, rows - 1) (\_ -> newArray (0, cols - 1) 0)

gridToST :: [[Int]] -> ST s (STGrid s)
gridToST grid = do
  let rows = length grid
  let cols = length $ head grid
  st <- newArray_ (0, rows - 1)
  initST st 0 grid rows cols
  return st
  where
    initST :: STGrid s -> Int -> [[Int]] -> Int -> Int -> ST s ()
    initST _ _ [] _ _ = return ()
    initST st rowIdx (r : rs) rows cols = do
      stRow <- newListArray (0, cols - 1) r
      writeArray st rowIdx stRow
      initST st (rowIdx + 1) rs rows cols
      return ()

stToGrid :: STGrid s -> ST s [[Int]]
stToGrid st = do
  rows <- getElems st
  forM rows $ \r -> getElems r

tick :: [[Int]] -> [[Int]]
tick grid = runST $ do
  let rows = length grid
  let cols = length $ head grid
  currST <- gridToST grid
  nextST <- emptyST rows cols
  forM_ [0 .. rows - 1] $ \rowIdx -> do
    nextRowST <- readArray nextST rowIdx
    forM_ [0 .. cols - 1] $ \colIdx -> do
      next <- nextVal currST rows cols rowIdx colIdx
      writeArray nextRowST colIdx next
  stToGrid nextST
  where
    nextVal currST rows cols rowIdx colIdx = do
      currVal :: Int <- readArray currST rowIdx >>= \row -> readArray row colIdx
      counter <- newSTRef 0
      forM_ neighbors $ \(neighborRowOffset, neighborColOffset) -> do
        let neighborRowIdx = rowIdx + neighborRowOffset
            neighborColIdx = colIdx + neighborColOffset
        when (neighborRowIdx >= 0 && neighborRowIdx < rows && neighborColIdx >= 0 && neighborColIdx < cols) $ do
          v <- readArray currST neighborRowIdx >>= \row -> readArray row neighborColIdx
          modifySTRef counter (+ v)
      liveNeighbors <- readSTRef counter
      return $ if currVal == 0 && liveNeighbors == 3 || currVal == 1 && (liveNeighbors == 2 || liveNeighbors == 3) then 1 else 0
