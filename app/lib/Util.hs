{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Util
  ( foldrM,
    foldrM_,
    getNodeEntityAtPosition,
    getSelectedBlock,
    setSelectedBlock,
    clearSelectedBlock,
    getClickedBlock,
    setClickedBlock,
    takeWhileInclusive,
  )
where

import Apecs
import Apecs.Experimental.Reactive
import Control.Monad (void)
import Types

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f d = foldr ((=<<) . f) (return d)

foldrM_ :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m ()
foldrM_ f d xs = void $ foldrM f d xs

getNodeEntityAtPosition :: GridPosition -> System World Entity
getNodeEntityAtPosition pos = do
  entitiesAtNodePos <- withReactive $ ordLookup pos
  case entitiesAtNodePos of
    [] -> newEntity (Node, pos)
    [e] -> return e
    _ : _ -> error "Very many entities"

setSelectedBlock :: GridPosition -> System World ()
setSelectedBlock pos = do
  nodeEntity <- getNodeEntityAtPosition pos
  nodeEntity $= Selected

getSelectedBlock :: System World (Maybe GridPosition)
getSelectedBlock = cfold (\_ (Selected, pos) -> Just pos) Nothing

clearSelectedBlock :: System World ()
clearSelectedBlock = cmap (\Selected -> Not @Selected)

setClickedBlock :: GridPosition -> System World ()
setClickedBlock pos = do
  nodeEntity <- getNodeEntityAtPosition pos
  nodeEntity $= Clicked

getClickedBlock :: System World (Maybe GridPosition)
getClickedBlock = cfold (\_ (Clicked, pos) -> Just pos) Nothing

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive p = foldr (\x ys -> if p x then x : ys else [x]) []