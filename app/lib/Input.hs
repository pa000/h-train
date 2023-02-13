{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Input (getNodeUnderCursor, handleInput) where

import Apecs
import Constants
import Control.Monad
import qualified Entity
import Foreign.C
import Linear.V2
import qualified Node
import qualified Raylib as RL
import Raylib.Types (Vector2 (Vector2))
import qualified Raylib.Types as RL
import Types

handleInput :: System World ()
handleInput = do
  clearClickedNode
  resetHoveredBlock
  handleKeyPresses
  handleMouseButtonPresses

getNodeUnderCursor :: System World Entity
getNodeUnderCursor = do
  Camera camera <- get global
  let RL.Vector2 (CFloat tx) (CFloat ty) = RL.camera2D'target camera
  let CFloat zoom = RL.camera2d'zoom camera
  mx <- liftIO RL.getMouseX
  my <- liftIO RL.getMouseY
  let x = floor $ (fromIntegral mx / zoom + tx - (cellSize / 2)) / cellSize
  let y = floor $ (fromIntegral my / zoom + ty - (cellSize / 2)) / cellSize
  Node.at (GridPosition (V2 x y))

handleMouseButtonPresses :: System World ()
handleMouseButtonPresses = do
  leftButton <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonLeft
  when leftButton handleLeftMouseButtonPress
  rightButtonDown <- liftIO $ RL.isMouseButtonDown RL.MouseButtonRight
  when rightButtonDown handleMiddleMouseButtonDown
  rightButtonPressed <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonRight
  when rightButtonPressed handleRightMouseButtonPress
  middleButton <- liftIO $ RL.isMouseButtonDown RL.MouseButtonMiddle
  when middleButton handleMiddleMouseButtonDown
  handleWheelMove

handleWheelMove :: System World ()
handleWheelMove = do
  Camera camera <- get global
  Vector2 (CFloat mx) (CFloat my) <- liftIO RL.getMousePosition
  let target = RL.camera2D'target camera
  let offset = RL.camera2D'offset camera
  let rotation = RL.camera2d'rotation camera
  let CFloat zoom = RL.camera2d'zoom camera
  wheelMove <- liftIO RL.getMouseWheelMove
  let zoom' = max (min (zoom + wheelMove / 10) 3) 0.5
  let mdx = mx / zoom - mx / zoom'
  let mdy = my / zoom - my / zoom'
  let target' = Vector2 (RL.vector2'x target + CFloat mdx) (RL.vector2'y target + CFloat mdy)
  let camera' = RL.Camera2D offset target' rotation (CFloat zoom')
  set global $ Camera camera'

handleMiddleMouseButtonDown :: System World ()
handleMiddleMouseButtonDown = do
  Camera camera <- get global
  dmouse <- liftIO RL.getMouseDelta
  let target = RL.camera2D'target camera
  let offset = RL.camera2D'offset camera
  let rotation = RL.camera2d'rotation camera
  let zoom = RL.camera2d'zoom camera
  let target' =
        RL.Vector2
          (RL.vector2'x target - RL.vector2'x dmouse / zoom)
          (RL.vector2'y target - RL.vector2'y dmouse / zoom)
  let camera' = RL.Camera2D offset target' rotation zoom
  set global $ Camera camera'

handleLeftMouseButtonPress :: System World ()
handleLeftMouseButtonPress = do
  hoveredNode <- getNodeUnderCursor
  Entity.setClicked hoveredNode

clearClickedNode :: System World ()
clearClickedNode = cmap (\Clicked -> Not @Clicked)

resetHoveredBlock :: System World ()
resetHoveredBlock = do
  hoveredNode <- getNodeUnderCursor
  Entity.setHovered hoveredNode

handleRightMouseButtonPress :: System World ()
handleRightMouseButtonPress = Entity.clearSelected

handleKeyPresses :: System World ()
handleKeyPresses = do
  key <- liftIO RL.getKeyPressed
  handleKeyPress key
  unless (key == RL.KeyNull) handleKeyPresses

handleKeyPress :: RL.KeyboardKey -> System World ()
handleKeyPress RL.KeyB = set global ToggleBuildMode
handleKeyPress RL.KeyS = set global ToggleBuildSignal
handleKeyPress RL.KeyD = set global ToggleDestructionMode
handleKeyPress _ = do return ()
