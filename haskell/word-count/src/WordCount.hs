{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module WordCount where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import qualified Data.List as L
import Data.Maybe
import Prelude hiding (words)

-- My own map

data Map k v = Null | Map k v (Map k v) (Map k v)

singleton :: (Ord k) => k -> v -> Map k v
singleton k v = Map k v Null Null

count :: (Ord k) => [k] -> Map k Int
count [] = Null
count xs = L.foldl' (\m k -> insertOrUpdate k 1 (+ 1) m) Null xs

insertOrUpdate :: (Ord k) => k -> v -> (v -> v) -> Map k v -> Map k v
insertOrUpdate k d _ Null = singleton k d
insertOrUpdate k d f (Map k' v left right) = case k `compare` k' of
  EQ -> Map k' (f v) left right
  LT -> Map k' v (insertOrUpdate k d f left) right
  GT -> Map k' v left (insertOrUpdate k d f right)

toList :: (Ord k) => Map k v -> [(k, v)]
toList Null = []
toList (Map k v left right) = toList left ++ [(k, v)] ++ toList right

---

newtype Parser r = Parser {runParser :: String -> Maybe (r, String)}

instance Functor Parser where
  f `fmap` (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure a = Parser (Just . (a,))
  f <*> g = do
    f' <- f
    f' <$> g

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p <|> Parser q = Parser $ \s ->
    case p s of
      Just r -> Just r
      _ -> q s

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \s -> do
    (p', r) <- p s
    let Parser q = f p'
    q r

char :: Parser Char
char = Parser $ \case
  [] -> Nothing
  (x : xs) -> Just (x, xs)

charSatisfies :: (Char -> Bool) -> Parser Char
charSatisfies pred = do
  ch <- char
  if pred ch then return ch else empty

apostrophe :: Parser Char
apostrophe = charSatisfies (== '\'')

upperChar :: Parser Char
upperChar = charSatisfies (`elem` ['A' .. 'Z'])

lowerChar :: Parser Char
lowerChar = charSatisfies (`elem` ['a' .. 'z'])

digit :: Parser Char
digit = charSatisfies (`elem` ['0' .. '9'])

wordChar :: Parser Char
wordChar = (toLower <$> upperChar) <|> lowerChar <|> digit

word :: Parser String
word = do
  void $ many apostrophe
  cha <- some wordChar
  apoSuf <- optional $ do
    apo <- apostrophe
    suf <- some wordChar
    return $ apo : suf

  void $ many apostrophe
  return $
    cha ++ case apoSuf of
      Just chb -> chb
      _ -> []

notP :: Parser Char -> Parser Char
notP p = do
  ch <- char
  case runParser p [ch] of
    Just _ -> empty
    Nothing -> return ch

spaces :: Parser ()
spaces = void $ many $ notP (wordChar <|> apostrophe)

words :: Parser [String]
words = do
  r <- many $ spaces >> word
  void spaces
  return r

wordCount :: String -> [(String, Int)]
wordCount xs = toList $ count $ fst $ fromJust $ runParser words xs
