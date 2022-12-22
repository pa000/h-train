{-# OPTIONS -Wall #-}

module Rendering (render, getScreenPos) where

import Apecs
import Apecs.Experimental.Reactive (ixLookup, withReactive)
import Apecs.System (cmapIf)
import Control.Exception (assert)
import Control.Monad
import qualified Data.Map as Map
import Data.Sequence
import Foreign.C (CFloat (CFloat))
import Input (getBlockUnderCursor)
import Linear (V2)
import Linear.V2 (V2 (..))
import qualified Raylib as RL
import qualified Raylib.Colors as RL
import Raylib.Types (Vector2 (..))
import Types
import Util
import Prelude hiding (last)

render :: System World ()
render = do
  liftIO $ do
    RL.beginDrawing
    RL.clearBackground RL.black

  state <- get global

  when (buildingMode state) $ do
    renderHoveredBlock
    selectedBlock <- getSelectedBlock
    maybe (return ()) renderBlock selectedBlock
    renderReachableBlocks
    renderBuildMode
  renderSectors

  liftIO RL.endDrawing

renderBlock :: GridPosition -> System World ()
renderBlock (GridPosition (V2 x y)) = do
  liftIO $ do
    RL.drawRectangle
      (x * cellSize + cellSize `div` 2)
      (y * cellSize + cellSize `div` 2)
      cellSize
      cellSize
      RL.lightGray

renderHoveredBlock :: System World ()
renderHoveredBlock = do
  GridPosition (V2 hoveredBlockX hoveredBlockY) <- getBlockUnderCursor
  liftIO $ do
    let x = hoveredBlockX * cellSize + cellSize `div` 2
    let y = hoveredBlockY * cellSize + cellSize `div` 2
    RL.drawRectangle x y cellSize cellSize RL.gray

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
renderSectors = cmapM_ renderSector

renderSector :: Sector -> System World ()
renderSector (Sector (prefix :|> last)) =
  foldrM_ renderTrack last prefix
renderSector (Sector Empty) = error "I know my invariants so this won't happen"

renderTrack :: GridPosition -> GridPosition -> System World GridPosition
renderTrack (GridPosition v1) (GridPosition v2) = do
  liftIO $ do
    RL.drawLineEx (getScreenPos v1) (getScreenPos v2) 3 RL.white
    RL.drawCircleV (getScreenPos v1) 1.5 RL.white
    RL.drawCircleV (getScreenPos v2) 1.5 RL.white
    return $ GridPosition v1

getScreenPos :: V2 Int -> Vector2
getScreenPos (V2 x y) =
  Vector2
    (CFloat . fromIntegral $ (x + 1) * cellSize)
    (CFloat . fromIntegral $ (y + 1) * cellSize)

renderReachableBlocks :: System World ()
renderReachableBlocks =
  cmapM_ (\(Reachable, Node, pos) -> renderBlock pos)