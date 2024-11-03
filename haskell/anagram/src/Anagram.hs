module Anagram (anagramsFor) where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Char
import GHC.Arr

countLetters :: String -> Array Int Int
countLetters s = runST $ do
  stArray <- newSTArray (0, 25) 0
  _ <- _countLetters s stArray
  freezeSTArray stArray
  where
    _countLetters [] stArray = return stArray
    _countLetters (x : xs) stArray = do
      let idx = fromEnum (toLower x) - fromEnum 'a'
      when (idx >= 0 && idx <= 25) $ do
        cnt <- readSTArray stArray idx
        writeSTArray stArray idx (cnt + 1)
      _countLetters xs stArray

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
  let xsLetterMap = countLetters xs
      letterMaps = map (id &&& countLetters) xss
   in map fst $ flip filter letterMaps $ \(w, letterMap) -> map toLower xs /= map toLower w && eqArray letterMap xsLetterMap
