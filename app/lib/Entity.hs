{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Entity where

import Apecs
import Types

setSelected :: Entity -> System World ()
setSelected entity = entity $= Selected

getSelected :: System World (Maybe Entity)
getSelected = cfold (\_ (Selected, e) -> Just e) Nothing

clearSelected :: System World ()
clearSelected = cmap (\Selected -> Not @Selected)

setClicked :: Entity -> System World ()
setClicked entity = entity $= Clicked

getClicked :: System World (Maybe Entity)
getClicked = cfold (\_ (Clicked, e) -> Just e) Nothing

getHovered :: System World (Maybe Entity)
getHovered = cfold (\_ (Hovered, e) -> Just e) Nothing

setHovered :: Entity -> System World ()
setHovered entity = entity $= Hovered

setReachable :: Entity -> System World ()
setReachable entity = entity $= Reachable

isReachable :: Entity -> System World Bool
isReachable entity = exists entity (Proxy @Reachable)

getPosition :: Entity -> System World GridPosition
getPosition = get

getNodeType :: Entity -> System World NodeType
getNodeType = get

isCoupled :: Entity -> System World Bool
isCoupled entity = exists entity (Proxy @CoupledTo)

getTrainPosition :: Entity -> System World Position
getTrainPosition = get

getCoupledTo :: Entity -> System World CoupledTo
getCoupledTo = get

hasSignal :: Entity -> System World Bool
hasSignal entity = exists entity (Proxy @Signal)

hasSignalTowards :: Entity -> Entity -> System World Bool
hasSignalTowards towards entity = do
  entityHasSignal <- hasSignal entity
  if entityHasSignal
    then do
      Signal _ towards' <- get entity
      return $ towards == towards'
    else return False

clearBusy :: Entity -> System World ()
clearBusy entity = entity $= Not @Busy

getSignal :: Entity -> System World Signal
getSignal = get

setSignal :: Bool -> Entity -> System World ()
setSignal green entity = entity $~ \(Signal _ e) -> Signal green e

setBusy :: Entity -> System World ()
setBusy entity = entity $= Busy

isBusy :: Entity -> System World Bool
isBusy entity = exists entity (Proxy @Busy)