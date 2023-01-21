{-# OPTIONS -Wall #-}

module Rendering (render) where

import Apecs
import Control.Monad
import Data.Foldable
import Data.Sequence
import qualified Data.Sequence as Seq
import qualified Entity
import Linear
import qualified Raylib as RL
import qualified Raylib.Colors as RL
import Raylib.Types (Vector2 (..))
import qualified Raylib.Types as RL
import Screen
import qualified Train
import Types
import Prelude hiding (last)

render :: System World ()
render = do
  liftIO $ do
    RL.beginDrawing
    RL.clearBackground RL.black

  state <- get global

  when (buildingMode state) $ do
    renderHoveredBlock
    selectedNode <- Entity.getSelected
    maybe (return ()) renderNode selectedNode
    renderReachableNodes
    renderBuildMode
  renderSectors
  renderJunctions
  unless (buildingMode state) $ do
    highlightHoveredSector

  renderTrains

  liftIO RL.endDrawing

renderTrains :: System World ()
renderTrains = cmapM_ renderTrain

renderTrain :: (Train, Position, Entity) -> System World ()
renderTrain (_, _, train) = do
  carPositions <- Train.calculateCarPositions train
  mapM_ renderCar carPositions

renderCar :: (V2 Float, V2 Float) -> System World ()
renderCar (startPos, endPos) = do
  let dir = endPos ^-^ startPos
  let rotation = Linear.unangle dir * 180 / pi
  let (Vector2 x y) = getScreenPosF startPos
  let rectangle = RL.Rectangle x y 15.0 10.0
  liftIO $ RL.drawRectanglePro rectangle (Vector2 0 5) rotation RL.red

highlightHoveredSector :: System World ()
highlightHoveredSector = do
  maybeHoveredBlock <- Entity.getHovered
  case maybeHoveredBlock of
    Nothing -> return ()
    Just hoveredBlock -> do
      hoveredSector <-
        Entity.getPosition hoveredBlock >>= getSectorContainingBlock
      forM_ hoveredSector renderHoveredSector

getSectorContainingBlock :: GridPosition -> System World (Maybe Sector)
getSectorContainingBlock block =
  cfold
    (\acc s@(Sector sector) -> if block `elem` sector then Just s else acc)
    Nothing

renderNode :: Entity -> System World ()
renderNode nodeEntity = do
  (GridPosition (V2 x y)) <- Entity.getPosition nodeEntity
  liftIO $ do
    RL.drawRectangle
      (x * cellSize + cellSize `div` 2)
      (y * cellSize + cellSize `div` 2)
      cellSize
      cellSize
      RL.lightGray

renderHoveredBlock :: System World ()
renderHoveredBlock = do
  maybeHoveredNode <- Entity.getHovered
  case maybeHoveredNode of
    Nothing -> return ()
    Just hoveredNode -> do
      GridPosition (V2 hoveredBlockX hoveredBlockY) <-
        Entity.getPosition hoveredNode
      state <- get global
      liftIO $ do
        let x = hoveredBlockX * cellSize + cellSize `div` 2
        let y = hoveredBlockY * cellSize + cellSize `div` 2
        RL.drawRectangle x y cellSize cellSize RL.gray
        when (placingSemaphore state) $ do
          RL.drawRectangle
            (x + cellSize `div` 4)
            (y + cellSize `div` 4)
            (cellSize `div` 2)
            (cellSize `div` 2)
            RL.green

renderBuildMode :: System World ()
renderBuildMode = do
  liftIO $ do
    h <- RL.getScreenHeight
    w <- RL.getScreenWidth
    let n = h `div` cellSize - 1
    let m = w `div` cellSize - 1
    mapM_ (renderHorizontalLine m) [0 .. n]
    mapM_ (renderVerticalLine n) [0 .. m]
  where
    renderHorizontalLine :: Int -> Int -> IO ()
    renderHorizontalLine m n = do
      RL.drawLine
        (cellSize `div` 2)
        (n * cellSize + cellSize `div` 2)
        (m * cellSize + cellSize `div` 2)
        (n * cellSize + cellSize `div` 2)
        RL.darkGray

    renderVerticalLine :: Int -> Int -> IO ()
    renderVerticalLine m n = do
      RL.drawLine
        (n * cellSize + cellSize `div` 2)
        (cellSize `div` 2)
        (n * cellSize + cellSize `div` 2)
        (m * cellSize + cellSize `div` 2)
        RL.darkGray

renderSectors :: System World ()
renderSectors = cmapM_ (renderSector RL.white)

renderSector :: RL.Color -> Sector -> System World ()
renderSector color (Sector (prefix :|> last)) = do
  _ <- foldrM (renderTrack color) last prefix
  return ()
renderSector _ (Sector Seq.Empty) = error "I know my invariants so this won't happen"

renderHoveredSector :: Sector -> System World ()
renderHoveredSector = renderSector RL.red

renderTrack :: RL.Color -> GridPosition -> GridPosition -> System World GridPosition
renderTrack color (GridPosition v1) (GridPosition v2) = do
  liftIO $ do
    RL.drawLineEx (getScreenPos v1) (getScreenPos v2) 3 color
    RL.drawCircleV (getScreenPos v1) 1.5 color
    RL.drawCircleV (getScreenPos v2) 1.5 color
    return $ GridPosition v1

renderJunctions :: System World ()
renderJunctions = cmapM_ renderJunction

renderJunction :: (NodeType, GridPosition) -> System World ()
renderJunction (Junction (GridPosition switchPos, GridPosition switchPos') _, GridPosition pos) = do
  liftIO $ RL.drawCircleV (getScreenPos pos) (cellSize / 3) RL.black
  liftIO $ RL.drawLineEx (getScreenPos pos) (getScreenPos switchPos) 3 RL.white
  liftIO $ RL.drawLineEx (getScreenPos pos) (getScreenPos switchPos') 3 RL.white
renderJunction _ = return ()

renderReachableNodes :: System World ()
renderReachableNodes =
  cmapM_ $ \(Reachable, Node, pos) -> renderNode pos
