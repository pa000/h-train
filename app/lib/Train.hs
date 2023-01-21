{-# OPTIONS -Wall #-}

module Train where

import Apecs
import Data.Foldable
import qualified Entity
import Linear (V2)
import qualified Node
import qualified Position
import qualified Raylib as RL
import Types

makeTrain :: Sector -> System World Entity
makeTrain (Sector sector) = do
  car1 <- newEntity Train
  car2 <- newEntity (Train, CoupledTo car1)
  car3 <- newEntity (Train, CoupledTo car2)
  car4 <- newEntity (Train, CoupledTo car3)
  newEntity (Train, Position (toList sector) 0.0, Speed 1, CoupledTo car4)

moveTrains :: System World ()
moveTrains = do
  cmapM updateTrainRoute
  cmapM moveTrain

updateTrainRoute ::
  (Train, Position) ->
  System World (Either Position (Not (Train, Position, Speed)))
updateTrainRoute (Train, Position [] _) = return $ Right Not
updateTrainRoute (Train, Position [_] _) = return $ Right Not
updateTrainRoute (Train, Position [currPos, nextPos] progress) = do
  nextNode <- Node.at nextPos
  currNode <- Node.at currPos
  neighbours <- Node.getNeighbours nextNode
  case find (/= currNode) neighbours of
    Nothing -> return $ Right Not
    Just neighbourNode -> do
      neighbourPos <- Entity.getPosition neighbourNode
      return $ Left $ Position [currPos, nextPos, neighbourPos] progress
updateTrainRoute (Train, pos) = return $ Left pos

moveTrain ::
  (Train, Position, Speed) ->
  System World (Either Position (Not (Train, Position, Speed)))
moveTrain (Train, pos, Speed speed) = do
  dt <- liftIO RL.getFrameTime
  newPosition <- Position.moveForward (speed * dt) pos
  return $ Left newPosition

calculateCarPositionsWithStartPosition ::
  Position -> Entity -> System World [(V2 Float, V2 Float)]
calculateCarPositionsWithStartPosition startPos car = do
  endPos <- Position.moveForward 0.5 startPos
  isCoupled <- Entity.isCoupled car
  let carPosition = (Position.onGrid startPos, Position.onGrid endPos)
  if isCoupled
    then do
      CoupledTo nextCar <- Entity.getCoupledTo car
      nextCarStartPos <- Position.moveForward 0.05 endPos
      restCarPositions <- calculateCarPositionsWithStartPosition nextCarStartPos nextCar
      return $ carPosition : restCarPositions
    else return [carPosition]

calculateCarPositions :: Entity -> System World [(V2 Float, V2 Float)]
calculateCarPositions train = do
  startPos <- Entity.getTrainPosition train
  calculateCarPositionsWithStartPosition startPos train
