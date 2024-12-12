{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Matrix
  ( Matrix,
    cols,
    column,
    flatten,
    fromList,
    fromString,
    reshape,
    row,
    rows,
    shape,
    transpose,
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as V

data Matrix a = Matrix {colSize :: Int, dt :: Vector a} deriving (Eq, Show)

cols :: Matrix a -> Int
cols Matrix {..} = colSize

column :: Int -> Matrix a -> Vector a
column x Matrix {..} =
  let l = length dt
   in V.fromList $ map (dt V.!) [x - 1, x - 1 + colSize .. l - 1]

flatten :: Matrix a -> Vector a
flatten Matrix {..} = dt

fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 V.empty
fromList ([] : _) = error "Invalid data"
fromList lists@(a : _) = Matrix {colSize = length a, dt = V.fromList (concat lists)}

fromString :: (Read a) => String -> Matrix a
fromString s =
  let lists = map (map read . words) (lines s)
   in fromList lists

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_, colSize) m = m {colSize}

row :: Int -> Matrix a -> Vector a
row x Matrix {..} = V.take colSize $ V.drop ((x - 1) * colSize) dt

rows :: Matrix a -> Int
rows Matrix {colSize = 0} = 0
rows Matrix {..} = V.length dt `div` colSize

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix =
  let rowSize = rows matrix
   in Matrix {colSize = rowSize, dt = V.concat $ map (`column` matrix) [1 .. cols matrix]}
