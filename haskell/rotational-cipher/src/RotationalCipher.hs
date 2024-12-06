module RotationalCipher (rotate) where

import Data.Char

ciphermap :: Int -> String
ciphermap n
  | n' <- n `mod` 26 =
    let plainmap = ['a'..'z']
        (l, r) = splitAt n' plainmap
        in r ++ l

toCipher :: String -> Char -> Char
toCipher cm ch
  | ch `elem` ['a'..'z'] = cm !! ((fromEnum ch - fromEnum 'a') :: Int)
  | ch `elem` ['A'..'Z'] = toUpper (toCipher cm $ toLower ch)
  | otherwise = ch

rotate :: Int -> String -> String
rotate n s = let
  cm = ciphermap n
  in map (toCipher cm) s
