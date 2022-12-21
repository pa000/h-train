{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Lib (main) where

import Apecs (System, get, global, liftIO, newEntity, newEntity_, runSystem, set)
import Control.Monad (unless, when)
import Data.Sequence (Seq (Empty), fromList)
import Linear.V2
import qualified Raylib as RL
import Raylib.Types (Vector2 (..))
import qualified Raylib.Types as RL
import Rendering (render)
import Types

main :: IO ()
main = initWorld >>= runSystem (initialise >> run >> terminate)

initialise :: System World ()
initialise = do
  let state = State {buildingMode = False, selectedBlock = Just $ V2 5 5}
  set global state

  newEntity_ $ Sector (V2 1 1) (fromList [V2 1 2, V2 2 3]) (V2 3 4)
  newEntity_ $ Sector (V2 4 3) (fromList [V2 3 2, V2 2 2]) (V2 1 1)

  liftIO $ do
    RL.initWindow 800 400 "h-train"
    RL.setTargetFPS 240

terminate :: System World ()
terminate = do
  liftIO RL.closeWindow

run :: System World ()
run = do
  update
  render
  shouldClose <- liftIO RL.windowShouldClose
  unless shouldClose run

update :: System World ()
update = do
  b <- liftIO $ RL.isKeyPressed RL.KeyB
  when b toggleBuildingMode

toggleBuildingMode :: System World ()
toggleBuildingMode = do
  state <- get global
  let state' = state {buildingMode = not $ buildingMode state}
  set global state'