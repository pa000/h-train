{-# OPTIONS -Wall #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Lib (main) where

import Apecs
import Control.Monad.Extra
import Data.Sequence (fromList)
import qualified Direction
import qualified Entity
import Input (handleInput)
import qualified Node
import qualified Raylib as RL
import Rendering (render)
import Train (makeTrain, moveTrains)
import Types
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
  clickedBlock <- Entity.getClicked
  selectedBlock <- Entity.getSelected
  case (clickedBlock, selectedBlock) of
    (Nothing, Nothing) -> Node.clearReachable
    (Just clickedEntity, Nothing) -> do
      Node.clearReachable
      Entity.setSelected clickedEntity
      Node.markReachableFrom clickedEntity
    (Nothing, Just _) -> return ()
    (Just clickedEntity, Just selectedEntity) -> do
      if clickedEntity == selectedEntity
        then Entity.clearSelected
        else do
          whenM (Entity.isReachable clickedEntity) $
            makeSector selectedEntity clickedEntity

          Node.clearReachable
          Entity.setSelected clickedEntity
          Node.markReachableFrom clickedEntity

makeSector :: Entity -> Entity -> System World ()
makeSector startNode endNode = do
  startPos <- Entity.getPosition startNode
  endPos <- Entity.getPosition endNode
  nodesInDir <-
    Node.getVisibleFromInDir
      startNode
      (Direction.getNormalized startPos endPos)
  let nodesInBetween = takeWhile (/= endNode) nodesInDir ++ [endNode]
  nodesPositions <- mapM Entity.getPosition (startNode : nodesInBetween)
  newEntity_ $ Sector (fromList nodesPositions)

  _ <- makeTrain (Sector (fromList nodesPositions))
  foldM_ makeTrack startNode nodesInBetween
  where
    makeTrack :: Entity -> Entity -> System World Entity
    makeTrack node next = do
      connect node next
      return next

connect :: Entity -> Entity -> System World ()
connect nodeEntity nodeEntity' = do
  pos' <- Entity.getPosition nodeEntity'
  nodeEntity $~ \(ConnectedTo neighbours) -> ConnectedTo $ pos' : neighbours

  pos <- Entity.getPosition nodeEntity
  nodeEntity' $~ \(ConnectedTo neighbours') -> ConnectedTo $ pos : neighbours'

  return ()
