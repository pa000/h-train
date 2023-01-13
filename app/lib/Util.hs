{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Util
  ( foldrM,
    foldrM_,
    spanM,
    getSelectedBlock,
    setSelectedBlock,
    clearSelectedBlock,
    getClickedBlock,
    setClickedBlock,
    getNodeEntityAtPosition,
    takeWhileInclusive,
  )
where

import Apecs
import Apecs.Experimental.Reactive
import Control.Monad
import Types

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f d = foldr ((=<<) . f) (return d)

foldrM_ :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m ()
foldrM_ f d xs = void $ foldrM f d xs

getNodeEntityAtPosition :: GridPosition -> System World Entity
getNodeEntityAtPosition pos = do
  entitiesAtNodePos <- withReactive $ ordLookup pos
  case entitiesAtNodePos of
    [] -> newEntity (Node, ConnectedTo [], pos)
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

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ [] = return ([], [])
spanM f (x : xs) = do
  b <- f x
  if b
    then do
      (rs, ls) <- spanM f xs
      return (x : rs, ls)
    else return ([], x : xs)

-- anyM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
-- anyM _ []