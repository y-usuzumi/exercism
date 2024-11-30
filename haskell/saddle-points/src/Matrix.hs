{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Matrix (saddlePoints, findSaddleLine) where

import Control.Monad
import qualified Data.Array as A
import Data.List

findSaddleLine :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int]
findSaddleLine cmp startIdx line = map fst $ _findSaddleLine line startIdx
  where
    _findSaddleLine [] _ = []
    _findSaddleLine (x : xs) idx =
      let tailResult = _findSaddleLine xs (idx + 1)
       in case tailResult of
            [] -> [(idx, x)]
            (t : _)
              | snd t == x -> (idx, x) : tailResult
              | cmp x (snd t) -> [(idx, x)]
              | otherwise -> tailResult

saddlePoints :: A.Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix =
  let ((rowLBound, colLBound), (rowUBound, colUBound)) = A.bounds matrix
      saddleRows = join $ map (\idx -> map (idx,) $ findSaddleLine (>) colLBound $ getRow idx) [rowLBound .. rowUBound]
      saddleCols = sort $ join $ map (\idx -> map (,idx) $ findSaddleLine (<) rowLBound $ getCol idx) [colLBound .. colUBound]
   in findSaddlePoints saddleRows saddleCols
  where
    getRow idx =
      let ((_, colLBound), (_, colUBound)) = A.bounds matrix
       in map ((matrix A.!) . (idx,)) [colLBound .. colUBound]
    getCol idx =
      let ((rowLBound, _), (rowUBound, _)) = A.bounds matrix
       in map ((matrix A.!) . (,idx)) [rowLBound .. rowUBound]
    findSaddlePoints [] _ = []
    findSaddlePoints _ [] = []
    findSaddlePoints (a : as) (b : bs)
      | a < b = findSaddlePoints as (b : bs)
      | a > b = findSaddlePoints (a : as) bs
      | otherwise = a : findSaddlePoints as bs