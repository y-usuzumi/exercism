module Knapsack (maximumValue) where

import Control.Monad.ST
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.STRef

type Cache = M.Map (Int, Int) (Maybe Int)

type CacheRef s = STRef s Cache

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue n items = runST $ do
  ref <- newSTRef M.empty
  fromMaybe 0 <$> _maximumValue n items 0 ref

_maximumValue :: Int -> [(Int, Int)] -> Int -> CacheRef s -> ST s (Maybe Int)
_maximumValue n _ _ _
  | n < 0 = return Nothing
_maximumValue _ [] _ _ = return (Just 0)
_maximumValue n (item : items) idx ref = do
  cache <- readSTRef ref
  case M.lookup (n, idx) cache of
    Just v -> return v
    _ -> do
      nextMaxTake <- fmap (snd item +) <$> _maximumValue (n - fst item) items (idx + 1) ref
      nextMaxNoTake <- _maximumValue n items (idx + 1) ref
      let result = max nextMaxTake nextMaxNoTake
      _ <- writeSTRef ref (M.insert (n, idx) result cache)
      return result
