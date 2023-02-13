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
  mapM_ (renderCar (getColor train)) carPositions
  renderPassengerCount train

renderPassengerCount :: Entity -> System World ()
renderPassengerCount train = do
  Passengers n <- get train
  carPositions <- Train.calculateCarPositions train
  let (startPos, endPos) = last carPositions
  let middle = (startPos ^+^ endPos) ^/ 2
  let Vector2 (CFloat x) (CFloat y) = getScreenPosF middle
  liftIO $ RL.drawText (show n) (fromEnum x) (fromEnum y - 5) 10 RL.white

renderHeadlights :: (V2 Float, V2 Float) -> System World ()
renderHeadlights (startPos, endPos) = do
  let dir = endPos ^-^ startPos
  let rotation = Linear.unangle dir * 180 / pi
  let center = getScreenPosF (endPos ^-^ dir ^/ 6)
  let color = RL.Color (CUChar 255) (CUChar 255) (CUChar 255) (CUChar 100)
  liftIO $ RL.drawCircleSector center 20 (- rotation + 40) (- rotation + 140) 10 color

renderCar :: RL.Color -> (V2 Float, V2 Float) -> System World ()
renderCar color (startPos, endPos) = do
  let dir = endPos ^-^ startPos
  let rotation = Linear.unangle dir * 180 / pi
  let (Vector2 x y) = getScreenPosF startPos
  let padding = 1
  let rectangle = RL.Rectangle x y ((cellSize / 2) - 2 * padding) ((cellSize / 3) - 2 * padding)
  let rectangleBoundary = RL.Rectangle x y (cellSize / 2) (cellSize / 3)
  liftIO $ RL.drawRectanglePro rectangleBoundary (Vector2 0 (cellSize / 6)) rotation RL.lightGray
  liftIO $ RL.drawRectanglePro rectangle (Vector2 (- padding) (cellSize / 6 - padding)) rotation color

getColor :: Entity -> RL.Color
getColor train =
  [RL.black, RL.darkPurple, RL.darkBlue, RL.orange, RL.darkGreen] !! (unEntity train `mod` 5)