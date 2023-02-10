module Rendering.UI where

import Apecs
import Linear
import qualified Raylib as RL
import qualified Raylib.Colors as RL
import Types

render :: System World ()
render = do
  renderButtons
  renderTexts

renderTexts :: System World ()
renderTexts = cmapM_ renderText

renderText :: (ScreenPosition, Text) -> System World ()
renderText (ScreenPosition (V2 x y), Text text size color) = do
  liftIO $ RL.drawText text x y size color

renderButtons :: System World ()
renderButtons = cmapM_ renderButton

renderButton :: Button -> System World ()
renderButton (ScreenPosition (V2 x y), ScreenSize (V2 w h), OnClick _) = do
  liftIO $ RL.drawRectangle x y w h RL.black
  liftIO $ RL.drawRectangleLines x y w h RL.white
