module Proverb(recite) where

import Data.List

recite :: [String] -> String
recite [] = ""
recite things = intercalate "\n" $ forWant things ++ ["And all for the want of a " ++ head things ++ "."]

forWant :: [String] -> [String]
forWant (a:b:as) = ("For want of a " ++ a ++ " the " ++ b ++ " was lost."):forWant (b:as)
forWant _ = []
