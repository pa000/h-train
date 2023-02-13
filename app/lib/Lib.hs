{-# OPTIONS -Wall #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib (main) where

import Apecs
import Control.Monad.Extra
import qualified Entity
import Input (getNodeUnderCursor, handleInput)
import qualified Node
import Random
import qualified Raylib as RL
import Raylib.Types
import qualified Raylib.Types as RL
import Rendering (render)
import qualified State
import Train (handleExpiredTimer, makeGhost, placeAt, placeGhostAt, updateTrains)
import Types
import Prelude hiding (last)

main :: IO ()
main = initWorld >>= runSystem (initialise >> run >> terminate)

initialise :: System World ()
initialise = do
  let state = None
  set global state

  let camera = Camera $ RL.Camera2D (Vector2 0 0) (Vector2 0 0) 0 1
  set global camera

  set global $ Timer 15

  set global $ Seed $ mkMRGen 5554

  set global $ Score 0
  set global $ LastScore 0

  set global $ Inventory 90 4
  _ <- Train.makeGhost

  liftIO $ do
    RL.initWindow 1200 600 "h-train"
    RL.setTargetFPS 240

  changeScene MainMenu

terminate :: System World ()
terminate = do
  liftIO RL.closeWindow

changeScene :: Scene -> System World ()
changeScene = set global

run :: System World ()
run = do
  update
  render
  shouldClose <- liftIO RL.windowShouldClose
  unless shouldClose run

handleActions :: System World ()
handleActions = do
  cmapM_ handleAction
  set global $ Not @Action

handleAction :: Action -> System World ()
handleAction StartGame = changeScene Game
handleAction ToggleBuildMode = State.toggleBuildingMode
handleAction ToggleBuildSignal = State.togglePlacingSignal
handleAction ToggleBuildTrain = State.togglePlacingTrain
handleAction ToggleDestructionMode = State.toggleDestructionMode
handleAction ToggleRemoveSignal = State.toggleRemovingSignal
handleAction _ = return ()

update :: System World ()
update = do
  action <- exists global (Proxy @Action)
  if action
    then do
      handleActions
      Entity.clearSelected
    else do
      handleInput
      handleClickedBlock
  clearBusyNodes
  updateTrains
  updateTimers
  checkExpiredTimers
  handleScore
  whenM State.isPlacingTrain updateTrainGhost

clearBusyNodes :: System World ()
clearBusyNodes = cmap $ \Busy -> Not @Busy

handleScore :: System World ()
handleScore = do
  Score s <- get global
  LastScore ls <- get global
  when (s /= ls) $ do
    set global $ LastScore s
    inventory <- get global
    let inventory' =
          inventory
            { tracks = tracks inventory + (s - ls) `div` 2,
              trains = trains inventory + (s - ls) `div` 10
            }
    set global inventory'

getTrainGhost :: System World (Maybe Entity)
getTrainGhost =
  cfold (\_ (Train, Ghost, e :: Entity) -> Just e) Nothing

updateTrainGhost :: System World ()
updateTrainGhost = do
  getTrainGhost >>= \case
    Nothing -> return ()
    Just trainGhost -> do
      hoveredNode <- Input.getNodeUnderCursor
      Node.getNeighbours hoveredNode >>= \case
        [] -> trainGhost $= Not @Position
        neighbour : _ ->
          Train.placeGhostAt trainGhost hoveredNode neighbour

checkExpiredTimers :: System World ()
checkExpiredTimers = do
  checkExpiredGlobalTimer
  checkExpiredTrainTimers
  checkExpiredTextTimers

checkExpiredTextTimers :: System World ()
checkExpiredTextTimers = cmapM_ checkExpiredTextTimer

checkExpiredTextTimer :: (Text, Timer, Entity) -> System World ()
checkExpiredTextTimer (Text _, Timer t, text) =
  when (t < 0) $ text $= Not @(Text, Timer, Position)

checkExpiredTrainTimers :: System World ()
checkExpiredTrainTimers = cmapM_ checkExpiredTrainTimer

checkExpiredTrainTimer :: (Train, Timer, Entity) -> System World ()
checkExpiredTrainTimer (Train, Timer t, train) = do
  when (t < 0) $ Train.handleExpiredTimer train

checkExpiredGlobalTimer :: System World ()
checkExpiredGlobalTimer = do
  Timer t <- get global
  when (t < 0) handleExpiredGlobalTimer

handleExpiredGlobalTimer :: System World ()
handleExpiredGlobalTimer = do
  cmap $ \(Station, Connected, Passengers n) -> Passengers (n + 1)
  set global $ Timer 15

updateTimers :: System World ()
updateTimers = cmapM updateTimer

updateTimer :: Timer -> System World Timer
updateTimer (Timer t) = do
  dt <- liftIO RL.getFrameTime
  return $ Timer (t - dt)

handleClickedBlock :: System World ()
handleClickedBlock = do
  clickedBlock <- Entity.getClicked
  selectedBlock <- Entity.getSelected
  buildingMode <- State.isBuildingMode
  placingSignal <- State.isPlacingSignal ||^ State.isRemovingSignal
  placingTrain <- State.isPlacingTrain
  destructionMode <- State.isDestructionMode
  case (clickedBlock, selectedBlock) of
    (Nothing, Nothing) -> Node.clearReachable
    (Just clickedEntity, Nothing) -> do
      if buildingMode
        then
          if placingSignal
            then Node.placeSignal clickedEntity
            else
              if placingTrain
                then placeTrain clickedEntity
                else do
                  Node.clearReachable
                  Entity.setSelected clickedEntity
                  if destructionMode
                    then Node.markDestructibleFrom clickedEntity
                    else Node.markReachableFrom clickedEntity
        else Node.handleClick clickedEntity
    (Nothing, Just _) -> return ()
    (Just clickedEntity, Just selectedEntity) -> do
      if clickedEntity == selectedEntity
        then Entity.clearSelected
        else do
          whenM (Entity.isReachable clickedEntity) $
            if destructionMode
              then removeSector selectedEntity clickedEntity
              else makeSector selectedEntity clickedEntity

          Node.clearReachable
          Entity.setSelected clickedEntity
          if destructionMode
            then Node.markDestructibleFrom clickedEntity
            else Node.markReachableFrom clickedEntity

placeTrain :: Entity -> System World ()
placeTrain node = do
  inventory <- get global
  when (trains inventory > 0) $ do
    getTrainGhost >>= \case
      Nothing -> return ()
      Just trainGhost -> do
        Node.getNeighbours node >>= \case
          [] -> trainGhost $= Not @Position
          neighbour : _ -> do
            placed <- Train.placeAt trainGhost node neighbour
            when placed $ do
              let inventory' = inventory {trains = trains inventory - 1}
              set global inventory'

              when (trains inventory' > 0) $ void Train.makeGhost

makeSector :: Entity -> Entity -> System World ()
makeSector startNode endNode = do
  nodesInBetween <- Node.getBetween startNode endNode

  -- whenM (Node.isEmpty startNode) $ do
  --   _ <- makeTrain startNode (head nodesInBetween)
  --   return ()
  foldM_ makeTrack startNode nodesInBetween
  where
    makeTrack :: Entity -> Entity -> System World Entity
    makeTrack node next = do
      inventory <- get global
      let inventory' = inventory {tracks = tracks inventory - 1}
      set global inventory'

      Node.connect node next
      return next

removeSector :: Entity -> Entity -> System World ()
removeSector startNode endNode = do
  nodesInBetween <- Node.getBetween startNode endNode
  foldM_ removeTrack startNode nodesInBetween
  where
    removeTrack :: Entity -> Entity -> System World Entity
    removeTrack node next = do
      inventory <- get global
      let inventory' = inventory {tracks = tracks inventory + 1}
      set global inventory'

      Node.disconnect node next
      return next