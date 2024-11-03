module Bob (responseFor) where

import Data.Char (isSpace)

data Voice = Question | Yell | YellQuestion | Silence | Else

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

voice :: String -> Voice
voice s
  | trim s == [] = Silence
voice s = _voice False False (trim s) where
    _voice False True [] = Yell
    _voice False True ('?':[]) = YellQuestion
    _voice _ _ ('?':[]) = Question
    _voice True _ (_:xs) = _voice True False xs
    _voice False allCapitalLetters (x:xs) =
        let isSilence = x `elem` [' ', '\n', '\t']
        in if x `elem` ['a'..'z'] then _voice True False xs else
            if x `elem` ['A'..'Z'] then _voice False True xs else _voice False allCapitalLetters xs
    _voice _ _ _ = Else
    


responseFor :: String -> String
responseFor xs = case voice xs of
    Question -> "Sure."
    Yell -> "Whoa, chill out!"
    YellQuestion -> "Calm down, I know what I'm doing!"
    Silence -> "Fine. Be that way!"
    Else -> "Whatever."