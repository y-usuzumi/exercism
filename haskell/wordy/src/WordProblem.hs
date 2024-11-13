{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module WordProblem (answer) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  f `fmap` (Parser a) = Parser (fmap (first f) . a)

instance Applicative Parser where
  pure a = Parser $ Just . (a,)
  f <*> g = do
    f' <- f
    f' <$> g

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser a <|> Parser b = Parser $ \s -> case a s of
    Nothing -> b s
    r -> r

instance Monad Parser where
  return = pure
  Parser a >>= f = Parser $ \s -> do
    (r, remaining) <- a s
    let (Parser g) = f r
    g remaining

takeOne :: Parser Char
takeOne = Parser $ \case
  (ch : chs) -> Just (ch, chs)
  _ -> Nothing

char :: Char -> Parser Char
char ch = do
  ch' <- takeOne
  if ch == ch' then return ch else empty

string :: String -> Parser String
string [] = return ""
string (s : ss) = do
  s' <- char s
  ss' <- string ss
  return (s' : ss')

anyOf :: [Char] -> Parser Char
anyOf chs = do
  ch <- takeOne
  if ch `elem` chs then return ch else empty

number :: Parser Integer
number = read <$> some (anyOf $ '-' : ['0' .. '9'])

operator :: Parser (Integer -> Integer -> Integer)
operator =
  (string "plus" >> return (+))
    <|> (string "minus" >> return (-))
    <|> (string "multiplied by" >> return (*))
    <|> (string "divided by" >> return div)

spaces :: Parser ()
spaces = void $ many $ char ' '

eof :: Parser ()
eof = Parser $ \case
  "" -> Just ((), "")
  _ -> Nothing

expression :: Parser Integer
expression = do
  spaces
  void $ string "What is"
  spaces
  n <- number
  spaces
  ns <- many $ do
    op <- operator
    spaces
    n' <- number
    spaces
    return (op, n')
  void $ string "?"
  eof
  return $ foldl' (\n (op, n') -> op n n') n ns

answer :: String -> Maybe Integer
answer problem = fst <$> runParser expression problem
