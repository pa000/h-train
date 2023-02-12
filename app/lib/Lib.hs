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
import Control.Monad.Random
import qualified Entity
import qualified Game
import Input (handleInput)
import qualified MainMenu
import qualified Node
import Random
import qualified Raylib as RL
import Raylib.Types
import qualified Raylib.Types as RL
import Rendering (render)
import qualified State
import Train (handleExpiredTimer, makeTrain, updateTrains)
import Types
import Prelude hiding (last)

main :: IO ()
main = initWorld >>= runSystem (initialise >> run >> terminate)

initialise :: System World ()
initialise = do
  let state = State False False False
  set global state

  let camera = Camera $ RL.Camera2D (Vector2 0 0) (Vector2 0 0) 0 1
  set global camera

  set global $ Timer 60

  set global $ Seed $ mkMRGen 243332

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
handleAction ToggleDestructionMode = State.toggleDestructionMode
handleAction _ = return ()

update :: System World ()
update = do
  action <- exists global (Proxy @Action)
  if action
    then handleActions
    else handleInput
  handleClickedBlock
  updateTrains
  updateTimers
  checkExpiredTimers

checkExpiredTimers :: System World ()
checkExpiredTimers = do
  checkExpiredGlobalTimer
  checkExpiredTrainTimers

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
  -- Timer t <- get global
  -- freeDeadEnds <-
  --   cfold
  --     ( \acc (nodeType, _ :: Not Busy, e :: Entity) ->
  --         case nodeType of
  --           DeadEnd n -> (e, n) : acc
  --           _ -> acc
  --     )
  --     []
  -- let n = length freeDeadEnds
  -- when (n > 0) $ do
  --   random <- liftIO $ getRandomR (0, n - 1)
  --   let (n, n') = freeDeadEnds !! random
  --   _ <- makeTrain n n'
  --   return ()
  set global $ Timer 60

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
  state <- get global
  case (clickedBlock, selectedBlock) of
    (Nothing, Nothing) -> Node.clearReachable
    (Just clickedEntity, Nothing) -> do
      if buildingMode state
        then
          if placingSignal state
            then Node.placeSignal clickedEntity
            else do
              Node.clearReachable
              Entity.setSelected clickedEntity
              if destructionMode state
                then Node.markDestructibleFrom clickedEntity
                else Node.markReachableFrom clickedEntity
        else Node.handleClick clickedEntity
    (Nothing, Just _) -> return ()
    (Just clickedEntity, Just selectedEntity) -> do
      if clickedEntity == selectedEntity
        then Entity.clearSelected
        else do
          whenM (Entity.isReachable clickedEntity) $
            if destructionMode state
              then removeSector selectedEntity clickedEntity
              else makeSector selectedEntity clickedEntity

          Node.clearReachable
          Entity.setSelected clickedEntity
          Node.markReachableFrom clickedEntity

makeSector :: Entity -> Entity -> System World ()
makeSector startNode endNode = do
  nodesInBetween <- Node.getBetween startNode endNode

  whenM (Node.isEmpty startNode) $ do
    _ <- makeTrain startNode (head nodesInBetween)
    return ()
  foldM_ makeTrack startNode nodesInBetween
  where
    makeTrack :: Entity -> Entity -> System World Entity
    makeTrack node next = do
      Node.connect node next
      return next

removeSector :: Entity -> Entity -> System World ()
removeSector startNode endNode = do
  nodesInBetween <- Node.getBetween startNode endNode
  foldM_ removeTrack startNode nodesInBetween
  where
    removeTrack :: Entity -> Entity -> System World Entity
    removeTrack node next = do
      Node.disconnect node next
      return next