{-# OPTIONS -Wall #-}

module Position where

import Apecs
import qualified Entity
import Linear (V2, (^*), (^+^), (^-^))
import qualified Linear
import qualified Node
import Types

onGrid :: Position -> System World (V2 Float)
onGrid (Position [] _) = return 0.0
onGrid (Position [_] _) = return 0.0
onGrid (Position ((from : curr : _)) progress) = do
  GridPosition fromPos <- Entity.getPosition from
  GridPosition currPos <- Entity.getPosition curr
  let dir = Linear.normalize $ toFloatVector currPos ^-^ toFloatVector fromPos
  return $ toFloatVector fromPos ^+^ (dir ^* progress)

toFloatVector :: (Integral a1, Num a2) => V2 a1 -> V2 a2
toFloatVector = fmap fromIntegral

moveForward :: Float -> Position -> System World Position
moveForward _ pos@(Position [] _) = return pos
moveForward _ pos@(Position [_] _) = return pos
moveForward step (Position [from, current] progress) = do
  rest <- Node.getNext from current
  case rest of
    Nothing -> return $ Position [from, current] (progress + step)
    Just restNode -> do
      let newRoute = [from, current, restNode]
      moveForward step (Position newRoute progress)
moveForward step (Position (from : current : rest) progress) = do
  distance <- Node.distance from current
  let newProgress = progress + step
  if newProgress > distance
    then moveForward (step - (distance - progress)) (Position (current : rest) 0.0)
    else return $ Position (from : current : rest) newProgress