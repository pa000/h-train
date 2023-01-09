{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Board
  ( markNodesReachableFrom,
    clearReachableNodes,
    getVisibleNodesInDir,
    getNormalizedDir,
    getNeighbours,
    addNeighbours,
  )
where

import Apecs
import Data.List
import qualified Data.Map as Map
import Linear.V2
import Rendering
import Types
import Util

getNeighbours :: Board -> GridPosition -> [GridPosition]
getNeighbours (Board board) pos = Map.findWithDefault [] pos board

addNeighbours :: GridPosition -> [GridPosition] -> System World ()
addNeighbours node neighbours =
  global $~ \(Board board) ->
    Board $ Map.insertWith (++) node neighbours board

isNodeEmpty :: Board -> GridPosition -> Bool
isNodeEmpty board node = null $ getNeighbours board node

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
  concat <$> mapM (getNodesReachableFromInDir startPos) allDirections

getNodesReachableFromInDir ::
  GridPosition -> Direction -> System World [GridPosition]
getNodesReachableFromInDir startPos dir = do
  board <- get global
  nodesInDir <- getVisibleNodesInDir startPos dir
  let (nodesBetween, nodesLeft) = span (isNodeEmpty board) nodesInDir
  case nodesLeft of
    [] -> return $ nodesBetween \\ getNeighbours board startPos
    nodeLeft : _ -> do
      let reachableNodes
            | any (isAngleLegal startPos nodeLeft) (getNeighbours board nodeLeft) =
              nodeLeft : nodesBetween
            | otherwise =
              nodesBetween
      return $ reachableNodes \\ getNeighbours board startPos

isAngleLegal :: GridPosition -> GridPosition -> GridPosition -> Bool
isAngleLegal start middle end =
  let dirStart = getNormalizedDir middle start
      dirEnd = getNormalizedDir end middle
   in elem dirEnd (getLegalDirs dirStart)
        || elem dirStart (getLegalDirs dirEnd)

getLegalDirs :: Direction -> [Direction]
getLegalDirs dir =
  case dir of
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
