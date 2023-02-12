{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Train where

import Apecs
import Apecs.System
import Control.Monad
import Control.Monad.Extra (notM)
import Data.Foldable
import Data.List ((\\))
import qualified Entity
import Extra
import Linear (V2)
import qualified Node
import qualified Position
import qualified Raylib as RL
import Types
import Prelude hiding (tail)

acceleration :: Float
acceleration = 0.2

makeTrain :: Entity -> Entity -> System World Entity
makeTrain from to = do
  car1 <- newEntity Train
  car2 <- newEntity (Train, CoupledTo car1)
  car3 <- newEntity (Train, CoupledTo car2)
  car4 <- newEntity (Train, CoupledTo car3)
  l <- getTrainLength car4
  newEntity (Train, Position from to (- (l + 0.55)), Speed 1, CoupledTo car4)

updateTrains :: System World ()
updateTrains = do
  cmapM removeTrainOutOfTrack
  cmapM updateTrainSpeed
  cmapM moveTrain

getBreakDistance :: Float -> Float
getBreakDistance speed = speed * speed / (2 * acceleration)

removeTrainOutOfTrack ::
  Position -> System World (Either () (Not (Position, Speed, Train)))
removeTrainOutOfTrack pos@(Position from curr progress) = do
  dist <- Node.distance from curr
  if progress > dist
    then do
      leaveTrack pos
      return $ Right Not
    else return $ Left ()

updateTrainSpeed :: (Train, Position, Entity) -> System World Speed
updateTrainSpeed (Train, _, train) = do
  ifM (shouldBrake train) (brake train) (accelerate train)

brake :: Entity -> System World Speed
brake train = do
  Speed speed <- get train
  dt <- liftIO RL.getFrameTime
  let speed' = max (speed - acceleration * dt) 0
  when (speed' == 0) $ handleStoppedTrain train
  return $ Speed speed'

accelerate :: Entity -> System World Speed
accelerate train = do
  Speed speed <- get train
  dt <- liftIO RL.getFrameTime
  return $ Speed $ min 1 (speed + acceleration * dt)

handleStoppedTrain :: Entity -> System World ()
handleStoppedTrain train = do
  pos <- calculateTrainFrontPosition train
  stoppedAtStation <- shouldBrakeBecauseOfStation pos &&^ notForced train
  hasTimer <- Entity.hasTimer train
  when (stoppedAtStation && not hasTimer) $ do
    train $= Timer 5
  stoppedAtDeadEnd <- shouldBrakeBecauseOfDeadEnd pos
  when (stoppedAtDeadEnd && not stoppedAtStation) $ do
    reverseTrain train

reverseTrain :: Entity -> System World ()
reverseTrain train = do
  Position from curr progress <- calculateTrainFrontPosition train
  dist <- Node.distance curr from
  train $= Position curr from (dist - progress)
  train $= Not @GoPast

handleExpiredTimer :: Entity -> System World ()
handleExpiredTimer train = do
  train $= Not @Timer

  pos@(Position _ curr _) <- calculateTrainFrontPosition train
  whenM (shouldBrakeBecauseOfStation pos) $ do
    train $= GoPast curr

shouldBrake :: Entity -> System World Bool
shouldBrake train = do
  Speed speed <- get train
  frontPos <- calculateTrainFrontPosition train
  let breakDistance = getBreakDistance speed - 0.5
  pos <- Position.moveForward breakDistance frontPos

  shouldBrakeBecauseOfSignal pos
    ||^ shouldBrakeBecauseOfDeadEnd pos
    ||^ (shouldBrakeBecauseOfStation pos &&^ notForced train)

notForced :: Entity -> System World Bool
notForced train = notM $ exists train (Proxy @GoPast)

shouldBrakeBecauseOfSignal :: Position -> System World Bool
shouldBrakeBecauseOfSignal (Position from curr _) = do
  hasSignal <- Entity.hasSignal curr
  if hasSignal
    then do
      Signal green towards <- Entity.getSignal curr
      if towards == from && not green
        then return True
        else return False
    else return False

shouldBrakeBecauseOfDeadEnd :: Position -> System World Bool
shouldBrakeBecauseOfDeadEnd (Position _ curr _) =
  Entity.getNodeType curr >>= \case
    DeadEnd _ -> return True
    _ -> return False

shouldBrakeBecauseOfStation :: Position -> System World Bool
shouldBrakeBecauseOfStation (Position from curr _) = do
  Node.getNext from curr >>= \case
    Nothing -> Entity.hasStation curr
    Just next -> Entity.hasStation curr &&^ notM (Entity.hasStation next)

moveTrain :: (Train, Position, Speed) -> System World Position
moveTrain (Train, pos@(Position from _ _), Speed speed) = do
  dt <- liftIO RL.getFrameTime
  pos'@(Position from' _ _) <- Position.moveForward (speed * dt) pos
  when (from /= from') $
    leaveTrack pos
  return pos'

calculateTrainFrontPosition :: Entity -> System World Position
calculateTrainFrontPosition train = do
  trainLength <- getTrainLength train
  startPos <- Entity.getTrainPosition train
  frontPos@(Position from _ _) <- Position.moveForward trainLength startPos
  whenM (notM $ notForced train) $ do
    GoPast node <- get train
    when (from == node) $
      train $= Not @GoPast
  enterTrack frontPos
  return frontPos

leaveTrack :: Position -> System World ()
leaveTrack (Position from curr _) = do
  Entity.getNodeType from >>= \case
    Junction _ _ -> do
      Node.getNext curr from >>= \case
        Nothing -> error ""
        Just opposite ->
          Node.markSectorFree from opposite
    Through _ _ -> do
      Node.getNext from curr >>= \case
        Nothing -> Node.markSectorFree curr from
        Just _ -> return ()
      Node.getNext curr from >>= \case
        Nothing -> error ""
        Just opposite -> do
          signalTowards <- Entity.hasSignalTowards opposite from
          when signalTowards $ Node.markSectorFree from opposite
    _ -> return ()

enterTrack :: Position -> System World ()
enterTrack (Position from curr _) = do
  Node.markSectorBusy from curr
  Entity.updateSignal False from

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
