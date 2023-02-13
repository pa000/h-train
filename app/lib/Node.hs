{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Node where

import Apecs
import Apecs.Experimental.Reactive
import Control.Monad
import Control.Monad.Extra (concatMapM, notM, unlessM, whenM, (&&^), (||^))
import Control.Monad.ListM
import Data.Foldable
import Data.List ((\\))
import Data.Maybe
import qualified Direction
import qualified Entity
import qualified GridPosition
import Linear
import Random
import Screen
import qualified State
import Types

isStation :: GridPosition -> System World Bool
isStation (GridPosition (V2 x y)) = do
  Seed seed <- get global
  let r = randomDoubleField seed (x `div` 5, y `div` 2)
  return $ r < 0.008

transferPassengersFromStationNode :: Entity -> System World Int
transferPassengersFromStationNode node = do
  Passengers n <- get node
  node $= Passengers 0
  return n

transferPassengersFromStation :: Entity -> System World Int
transferPassengersFromStation node = do
  stationNodes <- getStationNodes node
  counts <- mapM transferPassengersFromStationNode stationNodes
  return $ sum counts

getStationPassengerCount :: Entity -> System World Int
getStationPassengerCount node = do
  stationNodes <- getStationNodes node
  counts <- mapM (get >=> (\(Passengers n) -> return n)) stationNodes
  return $ sum counts

getMiddleStationNode :: Entity -> System World Entity
getMiddleStationNode node = do
  stationNodes <- getStationNodes node
  sorted <- sortByM compareNodes stationNodes
  return $ sorted !! 2
  where
    compareNodes n n' = do
      (GridPosition (V2 x _)) <- Entity.getPosition n
      (GridPosition (V2 x' _)) <- Entity.getPosition n'
      return $ compare x x'

getStationNodes :: Entity -> System World [Entity]
getStationNodes node = do
  station <- Entity.hasStation node
  if station
    then do
      neighbours <- getNeighbours node
      stationNodes <- concatMapM (getStationNodesFrom node) neighbours
      return $ node : stationNodes
    else return []

getStationNodesFrom :: Entity -> Entity -> System World [Entity]
getStationNodesFrom from node = do
  station <- Entity.hasStation node
  if station
    then do
      getNext from node >>= \case
        Nothing -> return [node]
        Just next -> do
          rest <- getStationNodesFrom node next
          return $ node : rest
    else return []

at :: GridPosition -> System World Entity
at pos = do
  entitiesAtNodePos <- withReactive $ ordLookup pos
  case entitiesAtNodePos of
    [] -> do
      station <- isStation pos
      if station
        then do
          node <- newEntity (Node, Empty, pos, Station, Passengers 0)
          getNeighbouringStationNodes pos >>= \case
            [] -> error "One station"
            [n] -> node $= DeadEnd n
            [n, n'] -> node $= Through n n'
            _ -> error "Many stations"
          return node
        else newEntity (Node, Empty, pos)
    [e] -> return e
    _ : _ -> error "Very many entities"

getNeighbouringStationNodes :: GridPosition -> System World [Entity]
getNeighbouringStationNodes (GridPosition (V2 x y)) = do
  let nodeLeftPos = GridPosition (V2 (x - 1) y)
  let nodeRightPos = GridPosition (V2 (x + 1) y)
  leftStation <- isStation nodeLeftPos
  rightStation <- isStation nodeRightPos
  let ret
        | leftStation && rightStation = [nodeLeftPos, nodeRightPos]
        | leftStation = [nodeLeftPos]
        | rightStation = [nodeRightPos]
        | otherwise = []
  mapM at ret

connect :: Entity -> Entity -> System World ()
connect node node' = do
  node' $~ addNeighbour node
  node $~ addNeighbour node'
  whenM (Entity.hasStation node) $ node $= Connected
  whenM (Entity.hasStation node') $ node' $= Connected

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
  Entity.setBusy from
  Entity.setBusy to
  getNext from to >>= \case
    Nothing -> return ()
    Just next ->
      mapSector Entity.setBusy p to next
  getNext to from >>= \case
    Nothing -> return ()
    Just opposite ->
      mapSector Entity.setBusy p' from opposite
  where
    p from curr = do
      Entity.isBusy curr
        ||^ ( getNext curr from >>= \case
                Nothing -> return False
                Just opposite -> Entity.hasSignalTowards opposite from
            )
    p' from curr = do
      Entity.isBusy curr
        ||^ Entity.hasSignalTowards curr from
        ||^ ( Entity.getNodeType curr >>= \case
                Junction _ _ -> return True
                _ -> return False
            )

markSectorFree :: Entity -> Entity -> System World ()
markSectorFree _ _ = return ()

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
  (nodesInBetween, nodesLeft) <-
    spanM (isConnectionLegalAndNodeEmpty startNode) nodesInDir
  reachableNodes <- do
    case nodesLeft of
      [] -> return nodesInBetween
      nodeLeft : _ -> do
        legal <- isConnectionLegal startNode nodeLeft
        if legal
          then return $ nodesInBetween ++ [nodeLeft]
          else return nodesInBetween
  inventory <- get global
  return $ take (tracks inventory) reachableNodes

isThrough :: Entity -> System World Bool
isThrough node =
  get node >>= \case
    Through _ _ -> return True
    _ -> return False

isEdgeOfStation :: Entity -> System World Bool
isEdgeOfStation node = do
  pos <- Entity.getPosition node
  stationNeighbours <- getNeighbouringStationNodes pos
  return $ length stationNeighbours == 1

getDestructibleFromInDir :: Entity -> Direction -> System World [Entity]
getDestructibleFromInDir startNode dir = do
  nodesInDir <- getVisibleFromInDir startNode dir
  (_, nodesInBetween, _) <-
    foldM isConnectedAndNotStation (startNode, [], True) nodesInDir
  return nodesInBetween
  where
    isConnectedAndNotStation (prevNode, xs, continuous) node = do
      neighbours <- getNeighbours prevNode
      notStation <- notM $ Entity.hasStation node
      if node `elem` neighbours && notStation && continuous
        then return (node, xs ++ [node], True)
        else return (node, xs, False)

mapSector :: (Entity -> System World ()) -> (Entity -> Entity -> System World Bool) -> Entity -> Entity -> System World ()
mapSector f p start to = do
  true <- p start to
  f start
  if true
    then return ()
    else do
      getNext start to >>= \case
        Nothing -> do
          f start
          f to
        Just next -> mapSector f p to next

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
    else
      (isEdgeOfStation endNode ||^ notM (Entity.hasStation endNode))
        &&^ anyM
          (isTurnLegal startNode endNode)
          endNeighbours

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
  removingSignal <- State.isRemovingSignal
  case (nodeType, removingSignal) of
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
      let nextLegalSwitch =
            ( case dropWhile (/= switch) (legalTurns ++ legalTurns) of
                [] -> head legalTurns
                xs -> head $ tail xs
            )
      node $= Junction (switch', nextLegalSwitch) neighbours
      when (nextLegalSwitch == switch) $ turnSwitch node
    _ -> return ()

toggleSignal :: Entity -> System World ()
toggleSignal node = do
  hasSignal <- Entity.hasSignal node
  when hasSignal $ do
    Signal green towards <- Entity.getSignal node
    if green
      then node $= Signal (not green) towards
      else do
        getNext towards node >>= \case
          Nothing -> node $= Signal (not green) towards
          Just next -> do
            whenM (checkRoute node next) $
              node $= Signal (not green) towards

checkRoute :: Entity -> Entity -> System World Bool
checkRoute from to = do
  busy <- Entity.isBusy to
  signal <- Entity.hasSignal to &&^ Entity.hasSignalTowards from to
  if busy
    then return False
    else
      if signal
        then return True
        else
          getNext from to >>= \case
            Nothing -> return True
            Just next ->
              Entity.getNodeType next >>= \case
                Junction (n, n') _ ->
                  if n == to || n' == to
                    then checkRoute to next
                    else return False
                _ -> checkRoute to next

handleClick :: Entity -> System World ()
handleClick node = do
  nodeType <- Entity.getNodeType node
  case nodeType of
    Through _ _ -> toggleSignal node
    Junction _ _ -> do
      unlessM (Entity.isBusy node) $
        turnSwitch node
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