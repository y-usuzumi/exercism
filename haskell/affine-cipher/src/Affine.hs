module Affine (decode, encode) where

import Data.List.Split
import Data.Maybe
import Data.Char

isValidKey :: (Int, Int) -> Bool
isValidKey (a, _) = gcd a 26 == 1

extendedEuclid :: Int -> Int -> (Int, Int, Int)
extendedEuclid a 0 = (a, 1, 0)
extendedEuclid a b =
    let (g, s, t) = extendedEuclid b (a `mod` b)
    in (g, t, s - (a `div` b) * t)

decode :: (Int, Int) -> String -> Maybe String
decode key cipherText
  | isValidKey key = Just $ mapMaybe (decodeChar key) cipherText
  | otherwise = Nothing
  where
    decodeChar (a, b) ch
      | ch == ' ' = Nothing
      | ch `elem` ['0'..'9'] = Just ch
      | otherwise = Just $ let
        y = fromEnum ch - fromEnum 'a'
        (_, x, _) = extendedEuclid a 26
        in toEnum $ fromEnum 'a' + ((y - b) * x) `mod` 26

encode :: (Int, Int) -> String -> Maybe String
encode key plainText
  | isValidKey key = Just $ unwords $ chunksOf 5 $ mapMaybe (encodeChar key) plainText
  | otherwise = Nothing
  where
    encodeChar (a, b) ch
      | ch `elem` ['0'..'9'] = Just ch
      | ch `elem` ['A'..'Z'] = encodeChar (a, b) (toLower ch)
      | ch `elem` ['a'..'z'] = Just $ let
        i = fromEnum ch - fromEnum 'a'
        in toEnum $ fromEnum 'a' + (a * i + b) `mod` 26
      | otherwise = Nothing

