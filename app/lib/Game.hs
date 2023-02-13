module Game where

import Apecs
import Control.Monad.Extra (ifM, whenM)
import qualified Raylib.Colors as RL
import qualified State
import Types
import qualified UI

runUI :: System World ()
runUI = do
  showInventory
  showScore

showScore :: System World ()
showScore = do
  Score s <- get global
  UI.addText True (show s) 100 50 20 RL.white

showInventory :: System World ()
showInventory = do
  inventory <- get global
  color <- ifM State.isPlacingTrack (return RL.green) (return RL.white)
  UI.addButton False (show (tracks inventory) ++ " track") 70 100 20 color (set global ToggleBuildMode) (set global ToggleDestructionMode)
  color <- ifM State.isPlacingTrain (return RL.green) (return RL.white)
  UI.addButton False (show (trains inventory) ++ " train") 70 150 20 color (set global ToggleBuildTrain) (return ())
  color <- ifM State.isPlacingSignal (return RL.green) (return RL.white)
  UI.addButton False "signal" 70 200 20 color (set global ToggleBuildSignal) (set global ToggleRemoveSignal)