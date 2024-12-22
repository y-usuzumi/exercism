module Cipher (offsetChar, caesarDecode, caesarEncode, caesarEncodeRandom) where
import System.Random
import Control.Monad (replicateM)

offsetChar :: Int -> Char -> Char
offsetChar offset = toEnum . (+ fromEnum 'a') . (\v -> if v < 0 then v + 26 else v) . (`mod` 26). (+offset) . subtract (fromEnum 'a') . fromEnum

caesarDecode :: String -> String -> String
caesarDecode keys = zipWith zf (cycle keys)
  where
    zf k = offsetChar (fromEnum 'a' - fromEnum k)

caesarEncode :: String -> String -> String
caesarEncode keys = zipWith zf (cycle keys)
  where
    zf k = offsetChar (fromEnum k - fromEnum 'a')

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  keys <- replicateM 1000 (toEnum . (+ fromEnum 'a') <$> randomRIO (0, 25))
  return (keys, caesarEncode keys text)
