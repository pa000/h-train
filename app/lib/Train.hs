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
import Prelude hiding (tail)

breakDeceleration :: Float
breakDeceleration = 0.2

makeTrain :: Sector -> System World Entity
makeTrain (Sector sector) = do
  car1 <- newEntity Train
  car2 <- newEntity (Train, CoupledTo car1)
  car3 <- newEntity (Train, CoupledTo car2)
  car4 <- newEntity (Train, CoupledTo car3)
  l <- getTrainLength car4
  newEntity (Train, Position (toList sector) (- (l + 0.55)), Speed 1, CoupledTo car4)

updateTrains :: System World ()
updateTrains = do
  cmapM updateTrainRoute
  cmapM updateTrainSpeed
  cmapM moveTrain

updateTrainRoute ::
  (Train, Position) ->
  System World (Either Position (Not (Train, Position, Speed)))
updateTrainRoute (Train, Position [] _) = return $ Right Not
updateTrainRoute (Train, Position [_] _) = return $ Right Not
updateTrainRoute (Train, Position [from, curr] progress) = do
  next <- Node.getNext from curr
  case next of
    Nothing -> do
      dist <- Node.distance from curr
      if progress > dist
        then return $ Right Not
        else return $ Left $ Position [from, curr] progress
    Just nextNode -> return $ Left $ Position [from, curr, nextNode] progress
updateTrainRoute (Train, pos) = return $ Left pos

getBreakDistance :: Float -> Float
getBreakDistance speed = speed * speed / (2 * breakDeceleration)

updateTrainSpeed :: (Train, Speed, Entity) -> System World Speed
updateTrainSpeed (Train, Speed speed, train) = do
  frontPos <- calculateTrainFrontPosition train
  let breakDistance = getBreakDistance speed - 0.5
  breakPos <- Position.moveForward breakDistance frontPos
  case breakPos of
    Position [] _ -> return $ Speed speed
    Position [_] _ -> return $ Speed speed
    Position (from : curr : _) _ -> do
      hasSignal <- Entity.hasSignal curr
      dt <- liftIO RL.getFrameTime
      if hasSignal
        then do
          Signal green towards <- Entity.getSignal curr
          if towards == from && not green
            then do
              let speed' = max (speed - breakDeceleration * dt) 0
              return $ Speed speed'
            else return $ Speed $ min 1 (speed + breakDeceleration * dt)
        else return $ Speed $ min 1 (speed + breakDeceleration * dt)

moveTrain :: (Train, Position, Speed) -> System World Position
moveTrain (Train, pos, Speed speed) = do
  dt <- liftIO RL.getFrameTime
  Position.moveForward (speed * dt) pos

calculateTrainFrontPosition :: Entity -> System World Position
calculateTrainFrontPosition train = do
  trainLength <- getTrainLength train
  startPos <- Entity.getTrainPosition train
  Position.moveForward trainLength startPos

getTrainLength :: Entity -> System World Float
getTrainLength train = do
  isCoupled <- Entity.isCoupled train
  if isCoupled
    then do
      CoupledTo tail <- Entity.getCoupledTo train
      tailLength <- getTrainLength tail
      return $ 0.5 + 0.05 + tailLength
    else return 0.5

calculateCarPositionsWithStartPosition ::
  Position -> Entity -> System World [(V2 Float, V2 Float)]
calculateCarPositionsWithStartPosition startPos car = do
  endPos <- Position.moveForward 0.5 startPos
  isCoupled <- Entity.isCoupled car
  startPosOnGrid <- Position.onGrid startPos
  endPosOnGrid <- Position.onGrid endPos
  let carPosition = (startPosOnGrid, endPosOnGrid)
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
