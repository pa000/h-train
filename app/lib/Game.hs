module Game where

import Apecs
import qualified Raylib.Colors as RL
import Types
import qualified UI

init :: System World ()
init = do
  initButtons

initButtons :: System World ()
initButtons = do
  UI.addButton "B" 50 50 20 RL.white ToggleBuildMode
