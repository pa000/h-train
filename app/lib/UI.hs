module UI where

import Apecs
import Control.Monad.Extra
import Foreign.C
import Linear
import qualified Raylib as RL
import qualified Raylib.Colors as RL
import Raylib.Types
import qualified Raylib.Types
import qualified Raylib.Types as RL
import Types

addText :: String -> Int -> Int -> Int -> Raylib.Types.Color -> System World ()
addText text x y size color = do
  font <- liftIO RL.getFontDefault
  Vector2 (CFloat textWidth) (CFloat textHeight) <-
    liftIO $ RL.measureTextEx font text (fromIntegral size) (fromIntegral size / 10)
  newEntity_ (Text text size color, ScreenPosition (V2 (x - fromEnum textWidth `div` 2) (y - fromEnum textHeight `div` 2)))

addButton :: String -> Int -> Int -> Int -> Raylib.Types.Color -> Action -> System World ()
addButton text textX textY textSize textColor action = do
  font <- liftIO RL.getFontDefault
  Vector2 (CFloat textWidth) (CFloat textHeight) <-
    liftIO $ RL.measureTextEx font text (fromIntegral textSize) (fromIntegral textSize / 10)
  newEntity_
    (OnClick action, ScreenPosition (V2 (textX - 10 - fromEnum textWidth `div` 2) (textY - 10 - fromEnum textHeight `div` 2)), ScreenSize (V2 (fromEnum textWidth + 20) (fromEnum textHeight + 20)))
  addText text textX textY textSize textColor

run :: System World ()
run = do
  update

update :: System World ()
update = do
  handleInput

handleInput :: System World ()
handleInput = do
  whenM (liftIO $ RL.isMouseButtonPressed RL.MouseButtonLeft) $ do
    mousePos <- liftIO RL.getMousePosition
    buttons <- cfold (mouseOverButton mousePos) []
    case buttons of
      [] -> return ()
      buttonAction : _ -> set global buttonAction
  where
    mouseOverButton (Vector2 (CFloat mouseX) (CFloat mouseY)) buttons (ScreenPosition (V2 x y), ScreenSize (V2 w h), OnClick buttonId) = do
      if fromEnum mouseX >= x
        && fromEnum mouseX <= x + w
        && fromEnum mouseY >= y
        && fromEnum mouseY <= y + h
        then buttonId : buttons
        else buttons

clear :: System World ()
clear = do
  clearButtons
  clearTexts

clearButtons :: System World ()
clearButtons =
  cmap clearButton

clearButton :: Button -> Not Button
clearButton _ = Not

clearTexts :: System World ()
clearTexts = cmap clearText

clearText :: (ScreenPosition, Text) -> Not (ScreenPosition, Text)
clearText _ = Not