module DND
  ( Character (..),
    ability,
    modifier,
    character,
  )
where

import Test.QuickCheck (Gen, choose)

default (Int, Double)

data Character = Character
  { strength :: Int,
    dexterity :: Int,
    constitution :: Int,
    intelligence :: Int,
    wisdom :: Int,
    charisma :: Int,
    hitpoints :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier = floor . (/ 2) . subtract 10 . fromIntegral

ability :: Gen Int
ability = do
  a <- choose (1, 6)
  b <- choose (1, 6)
  c <- choose (1, 6)
  d <- choose (1, 6)
  return $ (a + b + c + d) - minimum [a, b, c, d]

character :: Gen Character
character = do
  s <- ability
  d <- ability
  c <- ability
  i <- ability
  w <- ability
  ch <- ability
  let h = 10 + modifier c
  return $ Character s d c i w ch h
