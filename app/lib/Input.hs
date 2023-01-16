{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Input (getNodeUnderCursor, handleInput) where

import Apecs
import Control.Monad
import qualified Entity
import Linear.V2
import qualified Node
import qualified Raylib as RL
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
  mx <- liftIO RL.getMouseX
  my <- liftIO RL.getMouseY
  let x = (mx - cellSize `div` 2) `div` cellSize
  let y = (my - cellSize `div` 2) `div` cellSize
  Node.at (GridPosition (V2 x y))

handleMouseButtonPresses :: System World ()
handleMouseButtonPresses = do
  leftButton <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonLeft
  when leftButton handleLeftMouseButtonPress
  rightButton <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonRight
  when rightButton handleRightMouseButtonPress

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
handleKeyPress RL.KeyB = toggleBuildingMode
handleKeyPress _ = do return ()

toggleBuildingMode :: System World ()
toggleBuildingMode = do
  state <- get global
  let state' = state {buildingMode = not $ buildingMode state}
  set global state'