{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Board
  ( markNodesReachableFrom,
    clearReachableNodes,
    getVisibleNodesInDir,
    getNormalizedDir,
  )
where

import Apecs
import Control.Monad.Extra
import Data.List
import qualified Data.Map as Map
import Linear.V2
import Rendering
import Types
import Util

getVisibleNodesInDir :: GridPosition -> Direction -> System World [GridPosition]
getVisibleNodesInDir start dir = do
  let next = moveInDir start dir
  visible <- isVisibleOnScreen next
  if visible
    then do
      rest <- getVisibleNodesInDir next dir
      return $ next : rest
    else return []

normalizeDir :: Direction -> Direction
normalizeDir (V2 0 0) = V2 0 0
normalizeDir (V2 dx 0) = V2 (dx `div` abs dx) 0
normalizeDir (V2 0 dy) = V2 0 (dy `div` abs dy)
normalizeDir (V2 dx dy) = V2 (dx `div` abs dx) (dy `div` abs dy)

getNormalizedDir :: GridPosition -> GridPosition -> Direction
getNormalizedDir (GridPosition (V2 fromX fromY)) (GridPosition (V2 toX toY)) =
  normalizeDir (V2 (toX - fromX) (toY - fromY))

getNodesReachableFrom :: GridPosition -> System World [GridPosition]
getNodesReachableFrom startPos =
  concatMapM (getNodesReachableFromInDir startPos) allDirections

getNodesReachableFromInDir ::
  GridPosition -> Direction -> System World [GridPosition]
getNodesReachableFromInDir startPos dir = do
  nodesInDir <- getVisibleNodesInDir startPos dir
  (nodesBetween, nodesLeft) <- spanM isNodeEmptyAndAngleLegal nodesInDir
  case nodesLeft of
    [] -> return nodesBetween
    nodeLeft : _ -> do
      neighbours <- getNeighbours nodeLeft
      anyLegalAngle <-
        anyM (isAngleLegal startPos nodeLeft) neighbours
      let reachableNodes
            | anyLegalAngle = nodeLeft : nodesBetween
            | otherwise = nodesBetween
      startNeighbours <- getNeighbours startPos
      return $ reachableNodes \\ startNeighbours
  where
    isNodeEmptyAndAngleLegal pos = do
      startNeighbours <- getNeighbours startPos
      posNeighbours <- getNeighbours pos
      nodeIsEmpty <- isNodeEmpty pos
      anyAngleLegal <- anyM (isAngleLegal pos startPos) startNeighbours
      let correctNumberOfNeighbours =
            length startNeighbours < 4
              && length posNeighbours < 4
      return $
        nodeIsEmpty
          && (anyAngleLegal || null startNeighbours)
          && correctNumberOfNeighbours

getNeighbours :: GridPosition -> System World [GridPosition]
getNeighbours pos = do
  nodeEntity <- getNodeEntityAtPosition pos
  ConnectedTo neighbours <- get nodeEntity
  return neighbours

isNodeEmpty :: GridPosition -> System World Bool
isNodeEmpty pos = do
  neighbours <- getNeighbours pos
  return $ null neighbours

isAngleLegal :: GridPosition -> GridPosition -> GridPosition -> System World Bool
isAngleLegal start middle end = do
  let dirStart = getNormalizedDir middle start
      dirEnd = getNormalizedDir end middle
  return $
    elem dirEnd (getLegalDirs dirStart) || elem dirStart (getLegalDirs dirEnd)

getLegalDirs :: Direction -> [Direction]
getLegalDirs dir =
  case dir of
    V2 0 0 -> []
    V2 0 _ -> [V2 0 dy | dy <- [-1 .. 1]]
    V2 _ 0 -> [V2 dx 0 | dx <- [-1 .. 1]]
    V2 dx dy -> [V2 dx dy, V2 0 dy, V2 dx 0]

markNodesReachableFrom :: GridPosition -> System World ()
markNodesReachableFrom startPos = do
  reachableNodes <- getNodesReachableFrom startPos
  mapM_ markNodeAtPosition reachableNodes
  where
    markNodeAtPosition nodePosition = do
      nodeEntity <- getNodeEntityAtPosition nodePosition
      nodeEntity $= Reachable

allDirections :: [Direction]
allDirections =
  [V2 dx dy | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

clearReachableNodes :: System World ()
clearReachableNodes = cmap $ \Reachable -> Not @Reachable

moveInDir :: GridPosition -> Direction -> GridPosition
moveInDir (GridPosition (V2 x y)) (V2 dx dy) =
  GridPosition (V2 (x + dx) (y + dy))
