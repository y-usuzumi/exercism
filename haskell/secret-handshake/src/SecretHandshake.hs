module SecretHandshake (handshake) where

import Data.Function
import Data.List (foldl')

type Action = String

type ActionTransform = [Action] -> [Action]

actions :: [ActionTransform]
actions =
  [ prepend "wink",
    prepend "double blink",
    prepend "close your eyes",
    prepend "jump",
    reverse
  ]
  where
    prepend s = (s :)

handshake :: Int -> [String]
handshake n =
  let flips = getFlips n
      activeActions = zipWith (\flip action -> if flip then action else id) flips actions
   in reverse $ foldl' (&) {- (&) :: a -> (a -> b) -> b -} [] activeActions
  where
    getFlips 0 = []
    getFlips n
      | n < 0 = error "Impossible"
      | otherwise =
          let (d, m) = n `divMod` 2
           in (m == 1) : getFlips d
