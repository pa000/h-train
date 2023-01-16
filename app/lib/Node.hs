{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Node where

import Apecs
import Apecs.Experimental.Reactive
import Control.Monad.Extra (concatMapM, (&&^))
import Control.Monad.ListM
import qualified Entity
import Linear
import Rendering
import Types

at :: GridPosition -> System World Entity
at pos = do
  entitiesAtNodePos <- withReactive $ ordLookup pos
  case entitiesAtNodePos of
    [] -> newEntity (Node, ConnectedTo [], pos)
    [e] -> return e
    _ : _ -> error "Very many entities"

clearReachable :: System World ()
clearReachable = cmap $ \(Node, Reachable) -> Not @Reachable

markReachableFrom :: Entity -> System World ()
markReachableFrom startEntity = do
  reachableNodes <- getReachableFrom startEntity
  mapM_ Entity.setReachable reachableNodes

allDirections :: [Direction]
allDirections =
  [V2 dx dy | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

getReachableFrom :: Entity -> System World [Entity]
getReachableFrom startEntity =
  concatMapM (getReachableFromInDir startEntity) allDirections

getReachableFromInDir :: Entity -> Direction -> System World [Entity]
getReachableFromInDir startNode dir = do
  nodesInDir <- getVisibleFromInDir startNode dir
  (nodesInBetween, nodesLeft) <- spanM (isConnectionLegalAndNodeEmpty startNode) nodesInDir
  case nodesLeft of
    [] -> return nodesInBetween
    nodeLeft : _ -> do
      legal <- isConnectionLegal startNode nodeLeft
      if legal
        then return $ nodeLeft : nodesInBetween
        else return nodesInBetween

getVisibleFromInDir :: Entity -> Direction -> System World [Entity]
getVisibleFromInDir _ (V2 0 0) = return []
getVisibleFromInDir nodeEntity (V2 dx dy) = do
  GridPosition (V2 x y) <- Entity.getPosition nodeEntity
  let positionsInDir =
        [ GridPosition (V2 (x + dx * steps) (y + dy * steps))
          | steps <- [1 ..]
        ]
  visibleOnScreen <- takeWhileM isVisibleOnScreen positionsInDir
  mapM at visibleOnScreen

getNeighbours :: Entity -> System World [Entity]
getNeighbours nodeEntity = do
  ConnectedTo neighbours <- Entity.getConnectedTo nodeEntity
  mapM at neighbours

isEmpty :: Entity -> System World Bool
isEmpty nodeEntity = null <$> getNeighbours nodeEntity

isConnectionLegalAndNodeEmpty :: Entity -> Entity -> System World Bool
isConnectionLegalAndNodeEmpty startNode endNode = do
  endEmpty <- isEmpty endNode
  if not endEmpty
    then return False
    else isConnectionLegal startNode endNode

isConnectionLegal :: Entity -> Entity -> System World Bool
isConnectionLegal node node' =
  isDirectedConnectionLegal node node'
    &&^ isDirectedConnectionLegal node' node

isDirectedConnectionLegal :: Entity -> Entity -> System World Bool
isDirectedConnectionLegal startNode endNode = do
  endEmpty <- isEmpty endNode
  if endEmpty
    then return True
    else isDirectedConnectionToNonEmptyNodeLegal startNode endNode

isDirectedConnectionToNonEmptyNodeLegal :: Entity -> Entity -> System World Bool
isDirectedConnectionToNonEmptyNodeLegal startNode endNode = do
  endNeighbours <- getNeighbours endNode
  if startNode `elem` endNeighbours || length endNeighbours >= 4
    then return False
    else anyM (isAngleLegal startNode endNode) endNeighbours

isAngleLegal :: Entity -> Entity -> Entity -> System World Bool
isAngleLegal startNode middleNode endNode = do
  startPos <- Entity.getPosition startNode
  middlePos <- Entity.getPosition middleNode
  endPos <- Entity.getPosition endNode
  let dirStart = getNormalizedDir middlePos startPos
      dirEnd = getNormalizedDir endPos middlePos
  return $
    elem dirStart (getLegalTurns dirEnd) || elem dirEnd (getLegalTurns dirStart)

getNormalizedDir :: GridPosition -> GridPosition -> Direction
getNormalizedDir (GridPosition (V2 fromX fromY)) (GridPosition (V2 toX toY)) =
  normalizeDir (V2 (toX - fromX) (toY - fromY))

getLegalTurns :: Direction -> [Direction]
getLegalTurns dir =
  case dir of
    V2 0 0 -> []
    V2 0 _ -> [V2 0 dy | dy <- [-1 .. 1]]
    V2 _ 0 -> [V2 dx 0 | dx <- [-1 .. 1]]
    V2 dx dy -> [V2 dx dy, V2 0 dy, V2 dx 0]

normalizeDir :: Direction -> Direction
normalizeDir (V2 0 0) = V2 0 0
normalizeDir (V2 dx 0) = V2 (dx `div` abs dx) 0
normalizeDir (V2 0 dy) = V2 0 (dy `div` abs dy)
normalizeDir (V2 dx dy) = V2 (dx `div` abs dx) (dy `div` abs dy)
