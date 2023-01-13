{-# OPTIONS -Wall #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib (main) where

import Apecs
import Board
import Control.Monad (foldM_, unless, when)
import Data.Sequence (fromList)
import Input (handleInput)
import qualified Raylib as RL
import Rendering (render)
import Train (makeTrain, moveTrains)
import Types
import Util
import Prelude hiding (last)

main :: IO ()
main = initWorld >>= runSystem (initialise >> run >> terminate)

initialise :: System World ()
initialise = do
  let state = State {buildingMode = False}
  set global state

  liftIO $ do
    RL.initWindow 800 400 "h-train"
    RL.setTargetFPS 30

terminate :: System World ()
terminate = do
  liftIO RL.closeWindow

run :: System World ()
run = do
  update
  render
  shouldClose <- liftIO RL.windowShouldClose
  unless shouldClose run

update :: System World ()
update = do
  handleInput
  handleClickedBlock
  moveTrains

handleClickedBlock :: System World ()
handleClickedBlock = do
  clickedBlock <- getClickedBlock
  selectedBlock <- getSelectedBlock
  case (clickedBlock, selectedBlock) of
    (Nothing, Nothing) -> clearReachableNodes
    (Just pos, Nothing) -> do
      clearReachableNodes
      setSelectedBlock pos
      markNodesReachableFrom pos
    (Nothing, Just _) -> return ()
    (Just clickedPos, Just selectedPos) -> do
      if clickedPos == selectedPos
        then clearSelectedBlock
        else do
          clickedEntity <- getNodeEntityAtPosition clickedPos
          isReachable <- exists clickedEntity (Proxy @Reachable)

          when isReachable $ makeSector selectedPos clickedPos

          clearReachableNodes
          setSelectedBlock clickedPos
          markNodesReachableFrom clickedPos

makeSector :: GridPosition -> GridPosition -> System World ()
makeSector from to = do
  nodesInDir <- getVisibleNodesInDir from (getNormalizedDir from to)
  let nodesInBetween = takeWhileInclusive (/= to) nodesInDir
  newEntity_ $ Sector (fromList $ from : nodesInBetween)

  _ <- makeTrain $ Sector (fromList $ from : nodesInBetween)
  foldM_ makeTrack from nodesInBetween
  where
    makeTrack :: GridPosition -> GridPosition -> System World GridPosition
    makeTrack pos pos' = do
      connect pos pos'
      return pos'

connect :: GridPosition -> GridPosition -> System World ()
connect pos pos' = do
  nodeEntity <- getNodeEntityAtPosition pos
  nodeEntity' <- getNodeEntityAtPosition pos'
  ConnectedTo neighbours <- get nodeEntity
  ConnectedTo neighbours' <- get nodeEntity'
  nodeEntity $= ConnectedTo (pos' : neighbours)
  nodeEntity' $= ConnectedTo (pos : neighbours')
  return ()