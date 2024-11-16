module TwelveDays (recite) where

import Text.Printf

dayToOrdinal :: Int -> String
dayToOrdinal 1 = "first"
dayToOrdinal 2 = "second"
dayToOrdinal 3 = "third"
dayToOrdinal 4 = "fourth"
dayToOrdinal 5 = "fifth"
dayToOrdinal 6 = "sixth"
dayToOrdinal 7 = "seventh"
dayToOrdinal 8 = "eighth"
dayToOrdinal 9 = "ninth"
dayToOrdinal 10 = "tenth"
dayToOrdinal 11 = "eleventh"
dayToOrdinal 12 = "twelfth"
dayToOrdinal _ = error "Impossible"

gifts :: Int -> String
gifts 1 = "a Partridge in a Pear Tree"
gifts 2 = "two Turtle Doves, and " ++ gifts 1
gifts 3 = "three French Hens, " ++ gifts 2
gifts 4 = "four Calling Birds, " ++ gifts 3
gifts 5 = "five Gold Rings, " ++ gifts 4
gifts 6 = "six Geese-a-Laying, " ++ gifts 5
gifts 7 = "seven Swans-a-Swimming, " ++ gifts 6
gifts 8 = "eight Maids-a-Milking, " ++ gifts 7
gifts 9 = "nine Ladies Dancing, " ++ gifts 8
gifts 10 = "ten Lords-a-Leaping, " ++ gifts 9
gifts 11 = "eleven Pipers Piping, " ++ gifts 10
gifts 12 = "twelve Drummers Drumming, " ++ gifts 11
gifts _ = error "Impossible"

createLine :: Int -> String
createLine day = printf "On the %s day of Christmas my true love gave to me: %s." (dayToOrdinal day) (gifts day)

recite :: Int -> Int -> [String]
recite start stop
  | start == stop = [createLine start]
  | start > stop = error "Impossible"
  | otherwise = createLine start : recite (start + 1) stop
