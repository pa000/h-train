module MainMenu where

import Apecs
import Control.Monad.Extra
import Foreign.C
import qualified Raylib as RL
import qualified Raylib.Colors as RL
import Raylib.Types
import qualified Raylib.Types as RL
import Types
import qualified UI

init :: System World ()
init = do
  initTitle
  initButtons

initButtons :: System World ()
initButtons = do
  h <- liftIO RL.getScreenHeight
  w <- liftIO RL.getScreenWidth
  UI.addButton "play" (w `div` 2) (h `div` 2) 35 RL.white StartGame

initTitle :: System World ()
initTitle = do
  h <- liftIO RL.getScreenHeight
  w <- liftIO RL.getScreenWidth
  UI.addText "h-train" (w `div` 2) (h `div` 3) 70 RL.white
