{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Phone (number) where

import Control.Applicative (Alternative (..))
import Control.Arrow
import Control.Monad

newtype Parser a = Parser
  {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  Parser a >>= f = Parser $ \s -> case a s of
    Just (a', remaining) -> runParser (f a') remaining
    Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser a <|> Parser b = Parser $ \s -> a s <|> b s

takeOne :: Parser Char
takeOne = Parser $ \case
  [] -> Nothing
  (x : xs) -> Just (x, xs)

char :: Char -> Parser Char
char a = do
  ch <- takeOne
  if ch == a then return ch else empty

optional :: Parser a -> Parser (Maybe a)
optional a = (Just <$> a) <|> return Nothing

optional_ :: Parser a -> Parser ()
optional_ a = void (optional a)

clean :: Parser ()
clean = void $ many (allBut "0123456789")

eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
  _ -> Nothing

anyOf :: [Char] -> Parser Char
anyOf chs = do
  ch <- takeOne
  if ch `elem` chs then return ch else empty

allBut :: [Char] -> Parser Char
allBut chs = do
  ch <- takeOne
  if ch `elem` chs then empty else return ch

number :: String -> Maybe String
number xs =
  fst
    <$> runParser
      ( do
          optional_ (optional (char '+') >> char '1')
          clean
          d1 <- n
          d2 <- x
          d3 <- x
          clean
          d4 <- n
          d5 <- x
          d6 <- x
          clean
          d7 <- x
          d8 <- x
          d9 <- x
          d10 <- x
          clean
          eof
          return [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10]
      )
      xs
  where
    n = anyOf "23456789"
    x = anyOf "0123456789"
