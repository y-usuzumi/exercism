module DNA (toRNA) where

complement :: Char -> Either Char Char
complement 'G' = Right 'C'
complement 'C' = Right 'G'
complement 'T' = Right 'A'
complement 'A' = Right 'U'
complement a = Left a

toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA (a : as) = do
  ch <- complement a
  chrs <- toRNA as
  return (ch : chrs)