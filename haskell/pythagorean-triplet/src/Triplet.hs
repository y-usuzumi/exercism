module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [(a, b, sum-a-b)|a <- [1..sum], b <- [a..(sum-a) `div` 2], a*a + b * b == (sum-a-b) * (sum-a-b)]
