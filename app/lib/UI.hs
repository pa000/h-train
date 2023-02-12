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
  liftIO $ RL.drawText text (x - fromEnum textWidth `div` 2) (y - fromEnum textHeight `div` 2) size color

addButton :: String -> Int -> Int -> Int -> Raylib.Types.Color -> System World () -> System World ()
addButton text textX textY textSize textColor onClick = do
  font <- liftIO RL.getFontDefault
  Vector2 (CFloat textWidth) (CFloat textHeight) <-
    liftIO $ RL.measureTextEx font text (fromIntegral textSize) (fromIntegral textSize / 10)

  let x = textX - 10 - fromEnum textWidth `div` 2
  let y = textY - 10 - fromEnum textHeight `div` 2
  let w = fromEnum textWidth + 20
  let h = fromEnum textHeight + 20

  buttonHovered <- isButtonHovered x y w h
  let backgroundColor = if buttonHovered then RL.darkGray else RL.black

  liftIO $ RL.drawRectangle x y w h backgroundColor
  liftIO $ RL.drawRectangleLines x y w h RL.white
  addText text textX textY textSize textColor

  when buttonHovered $ do
    isMouseClicked <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonLeft
    when isMouseClicked onClick

isButtonHovered :: Int -> Int -> Int -> Int -> System World Bool
isButtonHovered x y w h = do
  mouseX <- liftIO RL.getMouseX
  mouseY <- liftIO RL.getMouseY
  return $ x <= mouseX && mouseX <= x + w && y <= mouseY && mouseY <= y + h
