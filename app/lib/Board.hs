{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Board (
  markNodesReachableFrom,
  clearReachableNodes,
  getNodesInDir,
  getNormalizedDir,
  getNeighbours,
  addNeighbours
)
where

import Apecs
import Apecs.Experimental.Reactive (ixLookup, ordLookup, withReactive)
import Control.Monad
import Data.Foldable
import qualified Data.Map as Map
import Data.Traversable (for)
import Foreign.C
import Linear.V2
import qualified Raylib as RL
import Raylib.Types
import Rendering
import Types
import Data.List
import Util

getNeighbours :: Board -> GridPosition -> [GridPosition]
getNeighbours (Board board) pos = Map.findWithDefault [] pos board

addNeighbours :: GridPosition -> [GridPosition] -> System World ()
addNeighbours node neighbours = do
  Board board <- get global
  let board' = Map.insertWith (++) node neighbours board
  modify global $ \(Board _) -> Board board'

getNodesInDir :: GridPosition -> Direction -> System World [GridPosition]
getNodesInDir start dir = do
  let next = moveInDir start dir
  visible <- isVisibleOnScreen next
  if visible
    then do
      rest <- getNodesInDir next dir
      return $ next : rest
    else return []

normalizeDir :: Direction -> Direction
normalizeDir (V2 dx 0) = V2 (dx `div` abs dx) 0
normalizeDir (V2 0 dy) = V2 0 (dy `div` abs dy)
normalizeDir (V2 dx dy) = V2 (dx `div` abs dx) (dy `div` abs dy)

getNormalizedDir :: GridPosition -> GridPosition -> Direction
getNormalizedDir (GridPosition (V2 fromX fromY)) (GridPosition (V2 toX toY)) =
  normalizeDir (V2 (toX - fromX) (toY - fromY))

getNodesReachableFrom :: GridPosition -> System World [GridPosition]
getNodesReachableFrom startPos = do
  concat <$> mapM (getNodesReachableFromInDir startPos) allDirections

getNodesReachableFromInDir ::
  GridPosition -> Direction -> System World [GridPosition]
getNodesReachableFromInDir startPos dir = do
  board <- get global
  nodesInDir <- getNodesInDir startPos dir
  return $ takeWhileInclusive (isReachable board) nodesInDir
    \\ getNeighbours board startPos
  where
    isReachable board node =
      let neighbours = getNeighbours board node
       in null neighbours -- || any (isAngleLegal startPos node) neighbours
    isAngleLegal start middle end =
      let dirStart = getNormalizedDir middle start
          dirEnd = getNormalizedDir end middle
       in elem dirStart (getLegalDirs dirEnd)
    getLegalDirs dir =
      case dir of
        V2 0 _ -> [V2 0 dy | dy <- [-1 .. 1]]
        V2 _ 0 -> [V2 dx 0 | dx <- [-1 .. 1]]
        V2 dx dy -> [V2 dx dy, V2 0 dy, V2 dx 0]

isVisibleOnScreen :: GridPosition -> System World Bool
isVisibleOnScreen (GridPosition pos) = do
  h <- liftIO RL.getScreenHeight
  w <- liftIO RL.getScreenWidth
  let (Vector2 (CFloat x) (CFloat y)) = getScreenPos pos
  return $ 0 <= x && x < fromIntegral w && 0 <= y && y < fromIntegral h

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
clearReachableNodes = cmap (\Reachable -> Not @Reachable)

moveInDir :: GridPosition -> Direction -> GridPosition
moveInDir (GridPosition (V2 x y)) (V2 dx dy) =
  GridPosition (V2 (x + dx) (y + dy))