{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Robot
  ( Bearing (East, North, South, West),
    bearing,
    coordinates,
    mkRobot,
    move,
  )
where

data Bearing
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

advanceStep :: Bearing -> (Integer, Integer)
advanceStep North = (0, 1)
advanceStep East = (1, 0)
advanceStep South = (0, -1)
advanceStep West = (-1, 0)

rotateL :: Bearing -> Bearing
rotateL North = West
rotateL East = North
rotateL South = East
rotateL West = South

rotateR :: Bearing -> Bearing
rotateR West = North
rotateR North = East
rotateR East = South
rotateR South = West

data Robot = Robot
  { direction :: Bearing,
    coords :: (Integer, Integer)
  }

bearing :: Robot -> Bearing
bearing = direction

coordinates :: Robot -> (Integer, Integer)
coordinates = coords

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords =
  Robot
    { direction,
      coords
    }

move :: Robot -> String -> Robot
move robot [] = robot
move robot@Robot {..} (ins : inss)
  | ins == 'L' = move robot {direction = rotateL direction} inss
  | ins == 'R' = move robot {direction = rotateR direction} inss
  | ins == 'A' = move robot {coords = coords +:+ advanceStep direction} inss
  | otherwise = error "Impossible"
  where
    (a, b) +:+ (c, d) = (a + c, b + d)
