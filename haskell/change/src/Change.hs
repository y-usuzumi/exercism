module Change (findFewestCoins) where

import Control.Monad.ST
import Data.List
import qualified Data.Map as M
import Data.Ord
import Data.STRef
import GHC.IO (unsafePerformIO)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = runST $ do
  cacheSTRef <- newSTRef M.empty
  result <- _findFewestCoins target (sortBy (comparing Down) coins) cacheSTRef
  return $ snd <$> result
  where
    _findFewestCoins :: Integer -> [Integer] -> STRef a (M.Map (Integer, Integer) (Maybe (Integer, [Integer]))) -> ST a (Maybe (Integer, [Integer]))
    _findFewestCoins 0 _ _ = return $ Just (0, [])
    _findFewestCoins n _ _
      | n < 0 = return Nothing
    _findFewestCoins _ [] _ = return Nothing
    _findFewestCoins target (c : cs) cacheSTRef = do
      cache <- readSTRef cacheSTRef
      case M.lookup (target, c) cache of
        Just n -> return n
        Nothing -> do
          result <-
            if c > target
              then _findFewestCoins target cs cacheSTRef
              else do
                a <- _findFewestCoins (target - c) (c : cs) cacheSTRef
                case a of
                  Nothing -> _findFewestCoins target cs cacheSTRef
                  Just (cntA, resA) -> do
                    b <- _findFewestCoins target cs cacheSTRef
                    case b of
                      Nothing -> return $ Just (cntA + 1, c : resA)
                      Just (cntB, resB) -> return $ if cntA < cntB then Just (cntA + 1, c : resA) else Just (cntB, resB)
          cache <- readSTRef cacheSTRef
          writeSTRef cacheSTRef $ M.insert (target, c) result cache
          return result