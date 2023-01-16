{-# OPTIONS -Wall #-}
module Train where

import Apecs
import Data.Foldable
import Data.Sequence
import Linear
import qualified Raylib as RL
import Raylib.Types
import Types

makeTrain :: Sector -> System World Entity
makeTrain sector = do
  newEntity (Train, Position sector 0.0, Speed 0.7)

sectorLength :: Sector -> Float
sectorLength (Sector Empty) = 0.0
sectorLength (Sector ((GridPosition first) :<| rest)) =
  fst $
    foldl'
      ( \(len, prevPos) (GridPosition nodePos) ->
          ( len + Linear.distance (toFloatVector prevPos) (toFloatVector nodePos),
            nodePos
          )
      )
      (0.0, first)
      rest

moveTrains :: System World ()
moveTrains = cmapM moveTrain

moveTrain :: (Train, Position, Speed) -> System World Position
moveTrain (Train, Position sector progress, Speed speed) = do
  dt <- liftIO RL.getFrameTime
  return $ Position sector (progress + speed * dt)

toFloatVector :: (Integral a1, Num a2) => V2 a1 -> V2 a2
toFloatVector = fmap fromIntegral

getPositionOnGrid :: Position -> V2 Float
getPositionOnGrid (Position (Sector Empty) _) = 0.0
getPositionOnGrid (Position (Sector (_ :<| Empty)) _) = 0.0
getPositionOnGrid (Position (Sector (start :<| next :<| rest)) progress) =
  if startNextDistance < progress
    then getPositionOnGrid (Position (Sector (next :<| rest)) (progress - startNextDistance))
    else
      let dir = Linear.normalize $ nextPos ^-^ startPos
       in startPos ^+^ (dir ^* progress)
  where
    startPos =
      let GridPosition v = start
       in toFloatVector v
    nextPos =
      let GridPosition v = next
       in toFloatVector v
    startNextDistance = Linear.distance startPos nextPos

getRotation :: Position -> Float
getRotation (Position (Sector Empty) _) = 0.0
getRotation (Position (Sector (_ :<| Empty)) _) = 0.0
getRotation (Position (Sector (start :<| next :<| _)) _) =
  let dir = nextPos ^-^ startPos
   in Linear.unangle dir * 180 / pi
  where
    startPos =
      let GridPosition v = start
       in toFloatVector v
    nextPos =
      let GridPosition v = next
       in toFloatVector v