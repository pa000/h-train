{-# OPTIONS -Wall #-}

module Rendering.Train where

import Apecs
import Constants (cellSize)
import Foreign.C
import Linear
import qualified Raylib as RL
import qualified Raylib.Colors as RL
import Raylib.Types
import qualified Raylib.Types as RL
import Screen
import qualified Train
import Types

renderAll :: System World ()
renderAll = cmapM_ render

render :: (Train, Position, Entity) -> System World ()
render (_, _, train) = do
  carPositions <- Train.calculateCarPositions train
  let front = last carPositions
  renderHeadlights front
  mapM_ renderCar carPositions

renderHeadlights :: (V2 Float, V2 Float) -> System World ()
renderHeadlights (startPos, endPos) = do
  let dir = endPos ^-^ startPos
  let rotation = Linear.unangle dir * 180 / pi
  let center = getScreenPosF (endPos ^-^ dir ^/ 6)
  let color = RL.Color (CUChar 255) (CUChar 255) (CUChar 255) (CUChar 100)
  liftIO $ RL.drawCircleSector center 20 (- rotation + 40) (- rotation + 140) 10 color

renderCar :: (V2 Float, V2 Float) -> System World ()
renderCar (startPos, endPos) = do
  let dir = endPos ^-^ startPos
  let rotation = Linear.unangle dir * 180 / pi
  let (Vector2 x y) = getScreenPosF startPos
  let padding = 1
  let rectangle = RL.Rectangle x y ((cellSize / 2) - 2 * padding) ((cellSize / 3) - 2 * padding)
  let rectangleBoundary = RL.Rectangle x y (cellSize / 2) (cellSize / 3)
  liftIO $ RL.drawRectanglePro rectangleBoundary (Vector2 0 (cellSize / 6)) rotation RL.lightGray
  liftIO $ RL.drawRectanglePro rectangle (Vector2 (- padding) (cellSize / 6 - padding)) rotation RL.darkPurple