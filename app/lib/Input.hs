{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Input (getBlockUnderCursor, handleInput) where

import Apecs
import Control.Monad
import Linear.V2
import qualified Raylib as RL
import qualified Raylib.Types as RL
import Types
import Util

handleInput :: System World ()
handleInput = do
  clearClickedBlock
  handleKeyPresses
  handleMouseButtonPresses

getBlockUnderCursor :: System World GridPosition
getBlockUnderCursor = do
  liftIO $ do
    mx <- RL.getMouseX
    my <- RL.getMouseY
    let x = (mx - cellSize `div` 2) `div` cellSize
    let y = (my - cellSize `div` 2) `div` cellSize
    return $ GridPosition (V2 x y)

handleMouseButtonPresses :: System World ()
handleMouseButtonPresses = do
  leftButton <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonLeft
  when leftButton handleLeftMouseButtonPress
  rightButton <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonRight
  when rightButton handleRightMouseButtonPress

handleLeftMouseButtonPress :: System World ()
handleLeftMouseButtonPress = do
  hoveredBlock <- getBlockUnderCursor
  setClickedBlock hoveredBlock

clearClickedBlock :: System World ()
clearClickedBlock = cmap (\Clicked -> Not @Clicked)

handleRightMouseButtonPress :: System World ()
handleRightMouseButtonPress = clearSelectedBlock

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