module Clock (addDelta, fromHourMin, toString) where

import Text.Printf

data Clock = Clock Int Int
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hours mins =
  let (hourCarry, newMins) = mins `divMod` 60
      newHours = (hours + hourCarry) `mod` 24
   in Clock newHours newMins

toString :: Clock -> String
toString (Clock hours mins) = printf "%02d:%02d" hours mins

addDelta :: Int -> Int -> Clock -> Clock
addDelta hourDiff minDiff (Clock hours mins) = fromHourMin (hours + hourDiff) (mins + minDiff)
