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

getConnectedTo :: Entity -> System World ConnectedTo
getConnectedTo = get