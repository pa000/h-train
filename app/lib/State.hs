{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}

module State where

import Apecs
import Control.Monad.Extra (ifM)
import Types

isBuildingMode :: System World Bool
isBuildingMode =
  get global >>= \case
    None -> return False
    _ -> return True

isDestructionMode :: System World Bool
isDestructionMode =
  get global >>= \case
    RemovingSignal -> return True
    RemovingTrack -> return True
    _ -> return False

isPlacingSignal :: System World Bool
isPlacingSignal =
  get global >>= \case
    PlacingSignal -> return True
    _ -> return False

isPlacingTrain :: System World Bool
isPlacingTrain =
  get global >>= \case
    PlacingTrain -> return True
    _ -> return False

isPlacingTrack :: System World Bool
isPlacingTrack =
  get global >>= \case
    PlacingTrack -> return True
    _ -> return False

isRemovingTrack :: System World Bool
isRemovingTrack =
  get global >>= \case
    RemovingTrack -> return True
    _ -> return False

isRemovingSignal :: System World Bool
isRemovingSignal =
  get global >>= \case
    RemovingSignal -> return True
    _ -> return False

toggleBuildingMode :: System World ()
toggleBuildingMode =
  ifM isPlacingTrack (set global None) (set global PlacingTrack)

togglePlacingSignal :: System World ()
togglePlacingSignal = do
  ifM isPlacingSignal (set global None) (set global PlacingSignal)

toggleDestructionMode :: System World ()
toggleDestructionMode = do
  ifM isRemovingTrack (set global None) (set global RemovingTrack)

togglePlacingTrain :: System World ()
togglePlacingTrain = do
  ifM isPlacingTrain (set global None) (set global PlacingTrain)

toggleRemovingSignal :: System World ()
toggleRemovingSignal = do
  ifM isRemovingSignal (set global None) (set global RemovingSignal)
