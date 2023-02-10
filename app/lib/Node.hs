{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Node where

import Apecs
import Apecs.Experimental.Reactive
import Control.Monad
import Control.Monad.Extra (concatMapM, (&&^))
import Control.Monad.ListM
import Data.Foldable
import Data.List ((\\))
import Data.Maybe
import qualified Direction
import qualified Entity
import Extra hiding (anyM)
import qualified GridPosition
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
  node' $~ addNeighbour node
  node $~ addNeighbour node'

disconnect :: Entity -> Entity -> System World ()
disconnect node node' = do
  node $~ removeNeighbour node'
  node' $~ removeNeighbour node
  node $= Not @Signal
  node' $= Not @Signal

addNeighbour :: Entity -> NodeType -> NodeType
addNeighbour neighbour Empty = DeadEnd neighbour
addNeighbour neighbour (DeadEnd neighbour') = Through neighbour' neighbour
addNeighbour neighbour (Through n n') = Junction (n, n') [n, n', neighbour]
addNeighbour neighbour (Junction (n, n') neighbours) =
  Junction (n, n') (neighbour : neighbours)

removeNeighbour :: Entity -> NodeType -> NodeType
removeNeighbour _ Empty = Empty
removeNeighbour neighbour (DeadEnd neighbour')
  | neighbour == neighbour' = Empty
  | otherwise = DeadEnd neighbour'
removeNeighbour neighbour (Through n n')
  | n == neighbour = DeadEnd n'
  | n' == neighbour = DeadEnd n
  | otherwise = Through n n'
removeNeighbour neighbour (Junction _ neighbours) =
  let neighbours' = neighbours \\ [neighbour]
   in case neighbours' of
        [m, m'] -> Through m m'
        m : m' : _ -> Junction (m, m') neighbours'
        _ -> error ""

clearReachable :: System World ()
clearReachable = cmap $ \(Node, Reachable) -> Not @Reachable

markDestructibleFrom :: Entity -> System World ()
markDestructibleFrom startNode = do
  reachableNodes <- getDestructibleFrom startNode
  mapM_ Entity.setReachable reachableNodes

markReachableFrom :: Entity -> System World ()
markReachableFrom startEntity = do
  reachableNodes <- getReachableFrom startEntity
  mapM_ Entity.setReachable reachableNodes

markSectorBusy :: Entity -> Entity -> System World ()
markSectorBusy from to = do
  sectorNodes <- getSector p from to
  mapM_ Entity.setBusy sectorNodes
  where
    p from curr = do
      getNext curr from >>= \case
        Nothing -> return False
        Just opposite -> Entity.hasSignalTowards opposite from

markSectorFree :: Entity -> Entity -> System World ()
markSectorFree from to = do
  sectorNodes <- getSector p from to
  mapM_ Entity.clearBusy sectorNodes
  where
    p from curr = do
      Entity.getNodeType curr >>= \case
        Through _ _ -> do
          getNext from curr >>= \case
            Nothing -> error ""
            Just opposite -> do
              signalTowards <- Entity.hasSignalTowards opposite curr
              if signalTowards
                then return True
                else return False
        Junction _ _ -> return True
        _ -> return False

allDirections :: [Direction]
allDirections =
  [V2 dx dy | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

getReachableFrom :: Entity -> System World [Entity]
getReachableFrom startEntity =
  concatMapM (getReachableFromInDir startEntity) allDirections

getDestructibleFrom :: Entity -> System World [Entity]
getDestructibleFrom startEntity =
  concatMapM (getDestructibleFromInDir startEntity) allDirections

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

isThrough :: Entity -> System World Bool
isThrough node =
  get node >>= \case
    Through _ _ -> return True
    _ -> return False

getDestructibleFromInDir :: Entity -> Direction -> System World [Entity]
getDestructibleFromInDir startNode dir = do
  nodesInDir <- getVisibleFromInDir startNode dir
  (_, nodesInBetween, _) <- foldM isConnected (startNode, [], True) nodesInDir
  return nodesInBetween
  where
    isConnected (prevNode, xs, continuous) node = do
      neighbours <- getNeighbours prevNode
      if node `elem` neighbours && continuous
        then return (node, xs ++ [node], True)
        else return (node, xs, False)

getSector :: (Entity -> Entity -> System World Bool) -> Entity -> Entity -> System World [Entity]
getSector p start to = do
  true <- p start to
  if true
    then return [start]
    else do
      getNext start to >>= \case
        Nothing -> return [start, to]
        Just next -> do
          rest <- getSector p to next
          return $ start : rest

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
  return $ case nodeType of
    Empty -> []
    DeadEnd neighbour -> [neighbour]
    Through neighbour neighbour' -> [neighbour, neighbour']
    Junction _ neighbours -> neighbours

getConnected :: Entity -> System World [Entity]
getConnected node = do
  nodeType <- Entity.getNodeType node
  return $ case nodeType of
    Empty -> []
    DeadEnd neighbour -> [neighbour]
    Through neighbour neighbour' -> [neighbour, neighbour']
    Junction (connectedNode, connectedNode') _ ->
      [connectedNode, connectedNode']

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

distance :: Entity -> Entity -> System World Float
distance node node' = do
  nodePos <- Entity.getPosition node
  node'Pos <- Entity.getPosition node'
  return $ GridPosition.distance nodePos node'Pos

placeSignal :: Entity -> System World ()
placeSignal node = do
  nodeType <- Entity.getNodeType node
  state <- get global
  case (nodeType, destructionMode state) of
    (Through neigh _, False) -> do
      hasSignal <- Entity.hasSignal node
      if hasSignal
        then do
          Signal green towards <- Entity.getSignal node
          towards' <- getNext towards node
          node $= Signal green (fromMaybe towards towards')
        else node $= Signal False neigh
    (Through _ _, True) -> node $= Not @Signal
    _ -> return ()

turnSwitch :: Entity -> System World ()
turnSwitch node = do
  nodeType <- Entity.getNodeType node
  case nodeType of
    Junction (switch, switch') neighbours -> do
      legalTurns <- filterM (isTurnLegal switch' node) neighbours
      let nextLegalSwitch = head $ tail $ dropWhile (/= switch) (legalTurns ++ legalTurns)
      node $= Junction (switch', nextLegalSwitch) neighbours
      when (nextLegalSwitch == switch) $ turnSwitch node
    _ -> return ()

toggleSignal :: Entity -> System World ()
toggleSignal node = do
  hasSignal <- Entity.hasSignal node
  when hasSignal $ do
    Signal green towards <- Entity.getSignal node
    node $= Signal (not green) towards

handleClick :: Entity -> System World ()
handleClick node = do
  nodeType <- Entity.getNodeType node
  case nodeType of
    Through _ _ -> toggleSignal node
    Junction _ _ -> turnSwitch node
    _ -> return ()

getBetween :: Entity -> Entity -> System World [Entity]
getBetween startNode endNode = do
  startPos <- Entity.getPosition startNode
  let GridPosition (V2 x y) = startPos
  endPos <- Entity.getPosition endNode
  let V2 dx dy = Direction.getNormalized startPos endPos
  let positionsInDir =
        [ GridPosition (V2 (x + dx * steps) (y + dy * steps))
          | steps <- [1 ..]
        ]
  let positionsInBetween = takeWhile (/= endPos) positionsInDir ++ [endPos]
  mapM at positionsInBetween