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

runUI :: System World ()
runUI = do
  addTitle
  addButtons

addButtons :: System World ()
addButtons = do
  h <- liftIO RL.getScreenHeight
  w <- liftIO RL.getScreenWidth
  UI.addButton True "play" (w `div` 2) (h `div` 2) 35 RL.white (set global StartGame) (return ())

addTitle :: System World ()
addTitle = do
  h <- liftIO RL.getScreenHeight
  w <- liftIO RL.getScreenWidth
  UI.addText True "h-train" (w `div` 2) (h `div` 3) 70 RL.white
