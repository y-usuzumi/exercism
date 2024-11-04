module CryptoSquare (encode) where

import Data.Char
import Data.List

normalize :: String -> String
normalize [] = []
normalize (x : xs)
  | x `elem` ['A' .. 'z'] || x `elem` ['a' .. 'z'] || x `elem` ['0' .. '9'] = toLower x : normalize xs
  | otherwise = normalize xs

toRectangle :: String -> [String]
toRectangle s =
  let l = length s
      lowerBound = floor (sqrt (fromIntegral l) :: Double) :: Int
      c =
        if lowerBound * lowerBound >= l then lowerBound else lowerBound + 1
   in transpose $ intoChunks c s
  where
    intoChunks _ [] = []
    intoChunks l s =
      let (chunk, res) = splitAt l s
       in padRight l chunk : intoChunks l res
    padRight l chunk =
      let chunkLen = length chunk
       in chunk ++ replicate (l - chunkLen) ' '

encode :: String -> String
encode xs
  | normalized@(_ : _) <- normalize xs =
      foldr1 (\l r -> l ++ " " ++ r) $ toRectangle normalized
  | otherwise = []
