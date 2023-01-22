{-# OPTIONS -Wall #-}

module Rendering.Train where

import Apecs
import Constants (cellSize)
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
  mapM_ renderCar carPositions

renderCar :: (V2 Float, V2 Float) -> System World ()
renderCar (startPos, endPos) = do
  let dir = endPos ^-^ startPos
  let rotation = Linear.unangle dir * 180 / pi
  let (Vector2 x y) = getScreenPosF startPos
  let rectangle = RL.Rectangle x y (cellSize / 2) (cellSize / 3)
  liftIO $ RL.drawRectanglePro rectangle (Vector2 0 (cellSize / 6)) rotation RL.red