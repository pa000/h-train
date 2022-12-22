{-# OPTIONS -Wall #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib (main) where

import Apecs (Proxy (..), System, exists, get, global, liftIO, newEntity, newEntity_, runSystem, set)
import Apecs.Experimental.Reactive
import Board
import Control.Monad (foldM_, guard, unless, when)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq (Empty), fromList)
import Input (getBlockUnderCursor, handleInput)
import Linear.V2
import qualified Raylib as RL
import Raylib.Types (Vector2 (..))
import qualified Raylib.Types as RL
import Rendering (render)
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
          if isReachable
            then do
              clearReachableNodes
              makeSector selectedPos clickedPos
              setSelectedBlock clickedPos
              markNodesReachableFrom clickedPos
            else do
              clearReachableNodes
              setSelectedBlock clickedPos
              markNodesReachableFrom clickedPos

makeSector :: GridPosition -> GridPosition -> System World ()
makeSector from to = do
  nodesInDir <- getNodesInDir from (getNormalizedDir from to)
  let nodesInBetween = from : takeWhileInclusive (/= to) nodesInDir
  newEntity_ $ Sector (fromList nodesInBetween)

  updateNeighbours nodesInBetween
  where
    updateNeighbours [] = return ()
    updateNeighbours [_] = return ()
    updateNeighbours (first : second : rest) = do
      addNeighbours second [first]
      addNeighbours first [second]
      updateNeighbours (second : rest)