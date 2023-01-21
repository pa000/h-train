{-# OPTIONS -Wall #-}

module State where

import Apecs
import Types

toggleBuildingMode :: System World ()
toggleBuildingMode = do
  state <- get global
  let state' = state {buildingMode = not $ buildingMode state}
  set global state'

togglePlacingSemaphore :: System World ()
togglePlacingSemaphore = do
  state <- get global
  let state' = state {placingSemaphore = not $ placingSemaphore state}
  set global state'