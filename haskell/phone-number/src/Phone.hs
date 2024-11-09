{-# LANGUAGE TupleSections #-}

module Phone (number) where

import qualified Data.List as L

newtype Parser s = Parser {unParser :: String -> (s, String)}

instance Functor Parser where
  f `fmap` Parser g = Parser $ \s -> let (r, remaining) = g s in (f r, remaining)

instance Applicative Parser where
  pure val = Parser (val,)
  Parser f <*> Parser g = Parser $ \s ->
    let (r, remaining) = g s
     in let (f', remaining') = f remaining in (f' r, remaining')

instance Monad Parser where
  return = pure
  Parser g >>= f = Parser $ \s ->
    let (val, remaining) = g s
        Parser f' = f val
     in f' remaining

take :: Int -> Parser String
take n = Parser $ L.splitAt n

number :: String -> Maybe String
number xs = error "You need to implement this function."
