{-# LANGUAGE MultiWayIf #-}
module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label r = let ohmsVal = ohms r
  in
    if | ohmsVal > 10 ^ 9 -> show (ohmsVal `div` 10^9) ++ " gigaohms"
       | ohmsVal > 10 ^ 6 -> show (ohmsVal `div` 10^6) ++ " megaohms"
       | ohmsVal > 10 ^ 3 -> show (ohmsVal `div` 10^3) ++ " kiloohms"
       | otherwise -> show ohmsVal ++ " ohms"

ohms :: Resistor -> Int
ohms Resistor {bands = (a, b, c)} =
  (fromEnum a * 10 + fromEnum b) * 10 ^ fromEnum c
