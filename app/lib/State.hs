{-# OPTIONS -Wall #-}

module State where

import Apecs
import Types

toggleBuildingMode :: System World ()
toggleBuildingMode = do
  state <- get global
  let state' = state {buildingMode = not $ buildingMode state}
  set global state'

togglePlacingSignal :: System World ()
togglePlacingSignal = do
  state <- get global
  let state' = state {placingSignal = not $ placingSignal state}
  set global state'

toggleDestructionMode :: System World ()
toggleDestructionMode = do
  state <- get global
  let state' = state {destructionMode = not $ destructionMode state}
  set global state'