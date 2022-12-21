{-# OPTIONS -Wall #-}

module Rendering (render) where

import Apecs
import Control.Monad (foldM_, when)
import Data.Sequence
import Linear (V2)
import Linear.V2 (V2 (..))
import qualified Raylib as RL
import qualified Raylib.Colors as RL
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
    maybe (return ()) renderSelectedBlock (selectedBlock state)
    renderBuildMode
    renderHoveredBlock
  renderSectors

  liftIO RL.endDrawing

renderSelectedBlock :: V2 Int -> System World ()
renderSelectedBlock (V2 x y) = do
  liftIO $ do
    RL.drawRectangle
      (x * cellSize + cellSize `div` 2)
      (y * cellSize + cellSize `div` 2)
      cellSize
      cellSize
      RL.darkGray

renderHoveredBlock :: System World ()
renderHoveredBlock = do
  liftIO $ do
    mx <- RL.getMouseX
    my <- RL.getMouseY
    let x = (mx - cellSize `div` 2) `div` cellSize * cellSize + cellSize `div` 2
    let y = (my - cellSize `div` 2) `div` cellSize * cellSize + cellSize `div` 2
    RL.drawRectangle x y cellSize cellSize RL.gray

renderBuildMode :: System World ()
renderBuildMode = do
  liftIO $ do
    m <- RL.getScreenHeight
    n <- RL.getScreenWidth
    mapM_ renderHorizontalLine [0 .. (m `div` cellSize) - 1]
    mapM_ renderVerticalLine [0 .. (n `div` cellSize) - 1]
  where
    renderHorizontalLine :: Int -> IO ()
    renderHorizontalLine n = do
      w <- RL.getScreenWidth
      RL.drawLine
        (cellSize `div` 2)
        (n * cellSize + cellSize `div` 2)
        (w - cellSize `div` 2)
        (n * cellSize + cellSize `div` 2)
        RL.lightGray

    renderVerticalLine :: Int -> IO ()
    renderVerticalLine n = do
      h <- RL.getScreenHeight
      RL.drawLine
        (n * cellSize + cellSize `div` 2)
        (cellSize `div` 2)
        (n * cellSize + cellSize `div` 2)
        (h - cellSize `div` 2)
        RL.lightGray

renderSectors :: System World ()
renderSectors = do
  cmapM_ renderSector

renderSector :: Sector -> System World ()
renderSector (Sector first middle last) = do
  foldrM_ renderTrack last (first :<| middle)

renderTrack :: V2 Int -> V2 Int -> System World (V2 Int)
renderTrack v1 v2 = do
  liftIO $ do
    RL.drawLineEx (v1 |* cellSize) (v2 |* cellSize) 3 RL.white
    RL.drawCircleV (v1 |* cellSize) 1.5 RL.white
    RL.drawCircleV (v2 |* cellSize) 1.5 RL.white
    return v1