{-# OPTIONS -Wall #-}

module Position where

import Apecs
import qualified Entity
import Linear (V2, (^*), (^+^), (^-^))
import qualified Linear
import qualified Node
import Types

onGrid :: Position -> System World (V2 Float)
onGrid (Position from curr progress) = do
  GridPosition fromPos <- Entity.getPosition from
  GridPosition currPos <- Entity.getPosition curr
  let dir = Linear.normalize $ toFloatVector currPos ^-^ toFloatVector fromPos
  return $ toFloatVector fromPos ^+^ (dir ^* progress)

toFloatVector :: (Integral a1, Num a2) => V2 a1 -> V2 a2
toFloatVector = fmap fromIntegral

moveForward :: Float -> Position -> System World Position
moveForward step (Position from current progress) = do
  let progress' = progress + step
  next <- Node.getNext from current
  case next of
    Nothing -> return $ Position from current progress'
    Just nextNode -> do
      dist <- Node.distance from current
      if progress' <= dist
        then return $ Position from current progress'
        else moveForward (progress' - dist) (Position current nextNode 0)