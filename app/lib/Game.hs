module Game where

import Apecs
import qualified Raylib.Colors as RL
import Types
import qualified UI

runUI :: System World ()
runUI = do
  showTimer
  addButtons

addButtons :: System World ()
addButtons = do
  UI.addButton "B" 50 50 20 RL.white (set global ToggleBuildMode)

showTimer :: System World ()
showTimer = do
  Timer t <- get global
  UI.addText (show $ round t) 100 50 20 RL.white
