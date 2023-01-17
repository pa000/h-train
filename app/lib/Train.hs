{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Train where

import Apecs
import Data.Foldable
import qualified Entity
import qualified Linear
import qualified Node
import qualified Raylib as RL
import Types

makeTrain :: Sector -> System World Entity
makeTrain (Sector sector) = do
  newEntity (Train, Position (toList sector) 0.0, Speed 0.7)

moveTrains :: System World ()
moveTrains = cmapM moveTrain

moveTrain :: (Train, Position, Speed) -> System World (Either Position (Not (Train, Position, Speed)))
moveTrain (Train, Position [] _, _) = return $ Left $ Position [] 0
moveTrain (Train, Position [_] _, _) = return $ Left $ Position [] 0
moveTrain (Train, Position [currPos, nextPos] progress, Speed speed) = do
  nextNode <- Node.at nextPos
  currNode <- Node.at currPos
  neighbours <- Node.getNeighbours nextNode
  case find (/= currNode) neighbours of
    Nothing -> return $ Right $ Not @(Train, Position, Speed)
    Just neighbourNode -> do
      neighbourPosition <- Entity.getPosition neighbourNode
      moveTrain (Train, Position [currPos, nextPos, neighbourPosition] progress, Speed speed)
moveTrain
  ( Train,
    Position (GridPosition currPos : GridPosition nextPos : rest) progress,
    Speed speed
    ) = do
    dt <- liftIO RL.getFrameTime
    let distance = Linear.distance (fmap fromIntegral currPos) (fmap fromIntegral nextPos)
    let newProgress = progress + speed * dt
    if newProgress > distance
      then moveTrain (Train, Position (GridPosition nextPos : rest) (newProgress - distance), Speed speed)
      else return $ Left $ Position (GridPosition currPos : GridPosition nextPos : rest) newProgress

-- calculateCarPositions :: Entity -> PositionState [(V2 Float, Float)]
-- calculateCarPositions train = do
--   startPos <- State.get
--   move 1.0
--   endPos <- State.get
--   let dir = endPos ^-^ startPos
--   let rotation = Linear.unangle dir * 180 / pi
--   let car = (startPos, rotation)
--   move 0.1
--   trainRest <- calculateCarPositions train
--   return $ car : trainRest