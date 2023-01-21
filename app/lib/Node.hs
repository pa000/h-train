{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Node where

import Apecs
import Apecs.Experimental.Reactive
import Control.Monad.Extra (concatMapM, (&&^))
import Control.Monad.ListM
import Data.Foldable
import qualified Direction
import qualified Entity
import Linear
import Screen
import Types

at :: GridPosition -> System World Entity
at pos = do
  entitiesAtNodePos <- withReactive $ ordLookup pos
  case entitiesAtNodePos of
    [] -> newEntity (Node, Empty, pos)
    [e] -> return e
    _ : _ -> error "Very many entities"

connect :: Entity -> Entity -> System World ()
connect node node' = do
  nodePos <- Entity.getPosition node
  node' $~ addNeighbour nodePos

  node'Pos <- Entity.getPosition node'
  node $~ addNeighbour node'Pos

addNeighbour :: GridPosition -> NodeType -> NodeType
addNeighbour neighbour Empty = DeadEnd neighbour
addNeighbour neighbour (DeadEnd neighbour') = Through neighbour' neighbour
addNeighbour neighbour (Through n n') = Junction (n, n') [n, n', neighbour]
addNeighbour neighbour (Junction (n, n') neighbours) =
  Junction (n, n') (neighbour : neighbours)

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
  nodeType <- Entity.getNodeType nodeEntity
  let neighbourPositions =
        ( case nodeType of
            Empty -> []
            DeadEnd neighbourPos -> [neighbourPos]
            Through neighbourPos neighbourPos' -> [neighbourPos, neighbourPos']
            Junction _ neighbours -> neighbours
        )
  mapM at neighbourPositions

getConnected :: Entity -> System World [Entity]
getConnected node = do
  nodeType <- Entity.getNodeType node
  let connectedPositions =
        ( case nodeType of
            Empty -> []
            DeadEnd neighbourPos -> [neighbourPos]
            Through neighbourPos neighbourPos' -> [neighbourPos, neighbourPos']
            Junction (connectedPos, connectedPos') _ ->
              [connectedPos, connectedPos']
        )
  mapM at connectedPositions

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
    else anyM (isTurnLegal startNode endNode) endNeighbours

isTurnLegal :: Entity -> Entity -> Entity -> System World Bool
isTurnLegal startNode middleNode endNode = do
  startPos <- Entity.getPosition startNode
  middlePos <- Entity.getPosition middleNode
  endPos <- Entity.getPosition endNode
  let dirStart = Direction.getNormalized startPos middlePos
      dirEnd = Direction.getNormalized middlePos endPos
  return $ dirEnd `elem` getLegalTurns dirStart

getLegalTurns :: Direction -> [Direction]
getLegalTurns (V2 0 0) = []
getLegalTurns (V2 0 dy) = [V2 dx dy | dx <- [-1 .. 1]]
getLegalTurns (V2 dx 0) = [V2 dx dy | dy <- [-1 .. 1]]
getLegalTurns (V2 dx dy) = [V2 dx dy, V2 0 dy, V2 dx 0]

getNext :: Entity -> Entity -> System World (Maybe Entity)
getNext from current = do
  connected <- getConnected current
  return $ find (/= from) connected