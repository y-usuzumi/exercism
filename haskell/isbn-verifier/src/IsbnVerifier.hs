{-# LANGUAGE LambdaCase #-}

module IsbnVerifier (isbn) where

import Control.Applicative
import Control.Monad
import Debug.Trace

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
  f `fmap` (Parser g) = Parser $ \s -> do
    (a, s') <- g s
    return (f a, s')

instance Applicative Parser where
  pure = return
  f <*> g = do
    f' <- f
    a <- g
    return (f' a)

instance Monad Parser where
  return a = Parser $ \s -> Right (a, s)
  Parser p >>= f = Parser $ \s -> do
    (a, s') <- p s
    let (Parser g) = f a
    g s'

instance Alternative Parser where
  empty = Parser $ const (Left "No parse")
  f <|> g = Parser $ \s -> case runParser f s of
    Left e -> runParser g s
    v -> v

parseError :: Parser e
parseError = Parser $ \_ -> Left "Parse error"

takeOne :: Parser Char
takeOne = Parser $ \case
  (a:as) -> return (a, as)
  _ -> Left "Parse error"

satisfies :: Bool -> Parser ()
satisfies pred = if pred then return () else parseError

takeDigit :: Parser Int
takeDigit = do
  ch <- takeOne
  optional $ takeOne >>= \ch -> satisfies (ch == '-')
  satisfies $ ch `elem` ['0'..'9']
  return $ read (ch:[])

takeDigitOrX :: Parser Int
takeDigitOrX = do
  ch <- takeOne
  (satisfies (ch `elem` ['0'..'9']) >> return (read (ch:[]))) <|>
    (satisfies (ch == 'X') >> return 10)

eof :: Parser ()
eof = Parser $ \case
  "" -> Right ((), "")
  _ -> Left "Parse error"

isbn :: String -> Bool
isbn s =
  case flip runParser s $ do
                             ds <- replicateM 9 takeDigit
                             dOrX <- takeDigitOrX
                             eof
                             let v = sum $ zipWith (*) (ds ++ [dOrX]) [10, 9..] in trace (show v) (return v)
  of
    Right (v, _) -> v `mod` 11 == 0
    _ -> False
