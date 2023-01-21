{-# OPTIONS -Wall #-}

module Position where

import Apecs
import qualified Entity
import qualified GridPosition
import Linear (V2, (^*), (^+^), (^-^))
import qualified Linear
import qualified Node
import Types

onGrid :: Position -> V2 Float
onGrid (Position [] _) = 0.0
onGrid (Position [_] _) = 0.0
onGrid (Position ((start : next : rest)) progress) =
  if startNextDistance < progress
    then onGrid (Position (next : rest) (progress - startNextDistance))
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
getRotation (Position [] _) = 0.0
getRotation (Position [_] _) = 0.0
getRotation (Position (start : next : _) _) =
  let dir = nextPos ^-^ startPos
   in Linear.unangle dir * 180 / pi
  where
    startPos =
      let GridPosition v = start
       in toFloatVector v
    nextPos =
      let GridPosition v = next
       in toFloatVector v

toFloatVector :: (Integral a1, Num a2) => V2 a1 -> V2 a2
toFloatVector = fmap fromIntegral

moveForward :: Float -> Position -> System World Position
moveForward _ pos@(Position [] _) = return pos
moveForward _ pos@(Position [_] _) = return pos
moveForward step (Position [from, current] progress) = do
  fromNode <- Node.at from
  currentNode <- Node.at current
  rest <- Node.getNext fromNode currentNode
  newRoute <- case rest of
    Nothing -> return []
    Just restNode -> do
      restPos <- Entity.getPosition restNode
      return [from, current, restPos]
  moveForward step (Position newRoute progress)
moveForward step (Position (from : current : rest) progress) = do
  let distance = GridPosition.distance from current
  let newProgress = progress + step
  if newProgress > distance
    then moveForward (step - (distance - progress)) (Position (current : rest) 0.0)
    else return $ Position (from : current : rest) newProgress