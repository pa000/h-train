{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Rendering (render) where

import Apecs
import Constants
import Control.Monad
import Control.Monad.Extra
import Data.Maybe
import qualified Entity
import Foreign.C
import GHC.Num
import qualified Game
import Linear ((^*), (^+^), (^-^))
import qualified Linear
import Linear.V2 (V2 (..))
import qualified MainMenu
import qualified Node
import Position (toFloatVector)
import qualified Raylib as RL
import qualified Raylib.Colors as RL
import Raylib.Types
import qualified Raylib.Types as RL
import qualified Rendering.Train as Train
import Screen
import qualified State
import Types
import Prelude hiding (last)

render :: System World ()
render = do
  liftIO $ do
    RL.beginDrawing
    RL.clearBackground RL.black

  get global >>= \case
    Game -> renderGame
    _ -> return ()

  runUI

  liftIO RL.endDrawing

runUI :: System World ()
runUI =
  get global >>= \case
    MainMenu -> MainMenu.runUI
    Game -> Game.runUI
    Pause -> MainMenu.runUI

renderGame :: System World ()
renderGame = do
  buildingMode <- State.isBuildingMode
  destructionMode <- State.isDestructionMode
  Camera camera <- get global
  liftIO $ do
    when destructionMode $
      RL.clearBackground (RL.Color (CUChar 64) (CUChar 14) (CUChar 10) (CUChar 255))
    RL.beginMode2D camera

  when buildingMode $ do
    renderHoveredBlock
    selectedNode <- Entity.getSelected
    maybe (return ()) renderNode selectedNode
    renderReachableNodes
    renderBuildMode
  renderTracks

  Train.renderAll
  renderSignals

  renderVisibleStations

  renderTexts

  liftIO $ RL.drawCircle 0 0 5 RL.purple

  liftIO RL.endMode2D

renderTexts :: System World ()
renderTexts = cmapM_ renderText

renderText :: (Text, Timer, ScreenPosition) -> System World ()
renderText (Text text, Timer t, pos) = do
  let ScreenPosition (Vector2 (CFloat x) (CFloat y)) = pos
  liftIO $ RL.drawText text (fromEnum x) (fromEnum $ y - (2 - t) * cellSize) 10 RL.green

renderVisibleStations :: System World ()
renderVisibleStations = do
  Camera camera <- get global
  let Vector2 (CFloat tx) (CFloat ty) = RL.camera2D'target camera
  let CFloat zoom = RL.camera2d'zoom camera
  let dx = fromEnum tx `div` cellSize - 1
  let dy = fromEnum ty `div` cellSize - 1
  h <- liftIO RL.getScreenHeight
  w <- liftIO RL.getScreenWidth
  let n = fromEnum (fromIntegral h / (cellSize * zoom)) + 1
  let m = fromEnum (fromIntegral w / (cellSize * zoom)) + 1
  mapM_ (renderStationsInRow dx m) [dy .. n + dy]
  where
    renderStationsInRow z m n = do
      mapM_ (renderStation n) [z .. m + z]
    renderStation y x = do
      node <- Node.at (GridPosition (V2 x y))
      whenM (Entity.hasStation node) $ do
        whenM (Node.getMiddleStationNode node >>= \m -> return $ m == node) $ do
          n <- Node.getStationPassengerCount node
          let Vector2 (CFloat sx) (CFloat sy) =
                getScreenPosF $ getCountPos (V2 x y)
          liftIO $ RL.drawText (show n) (fromEnum sx) (fromEnum sy) 10 RL.white
        nodeLeft <- Node.at (GridPosition (V2 (x - 1) y))
        nodeRight <- Node.at (GridPosition (V2 (x + 1) y))
        whenM (Entity.hasStation nodeLeft) $ do
          renderPlatform (V2 x y) (V2 (x - 1) y)
        whenM (notM $ Entity.hasStation nodeLeft) $ do
          renderPlatformEnd (V2 x y)
        whenM (Entity.hasStation nodeRight) $ do
          renderPlatform (V2 x y) (V2 (x + 1) y)
        whenM (notM $ Entity.hasStation nodeRight) $ do
          renderPlatformEnd (V2 x y)
      where
        renderPlatformEnd pos = do
          liftIO $
            RL.drawLineV
              (getScreenPosF $ getPlatformPos pos)
              (getScreenPosF $ getPlatformEndPos pos)
              RL.white
        renderPlatform pos pos' = do
          let nodePos = getPlatformPos pos
          let nodeLeftPos = getPlatformPos pos'
          liftIO $
            RL.drawLineV
              (getScreenPosF nodePos)
              (getScreenPosF nodeLeftPos)
              RL.white
        getPlatformPos (V2 x y)
          | even y = V2 (fromIntegral x) (fromIntegral y - 0.3)
          | otherwise = V2 (fromIntegral x) (fromIntegral y + 0.3)
        getPlatformEndPos (V2 x y)
          | even y = V2 (fromIntegral x) (fromIntegral y - 0.5)
          | otherwise = V2 (fromIntegral x) (fromIntegral y + 0.5)
        getCountPos (V2 x y)
          | even y = V2 (fromIntegral x) (fromIntegral y - 0.6)
          | otherwise = V2 (fromIntegral x) (fromIntegral y + 0.3)

renderNode :: Entity -> System World ()
renderNode nodeEntity = do
  GridPosition (V2 x y) <- Entity.getPosition nodeEntity
  liftIO $ do
    RL.drawRectangle
      (x * cellSize + cellSize `div` 2)
      (y * cellSize + cellSize `div` 2)
      cellSize
      cellSize
      RL.lightGray

renderHoveredBlock :: System World ()
renderHoveredBlock = do
  Entity.getHovered >>= \case
    Nothing -> return ()
    Just hoveredNode -> do
      hovPos@(GridPosition (V2 hoveredBlockX hoveredBlockY)) <-
        Entity.getPosition hoveredNode
      let x = hoveredBlockX * cellSize + cellSize `div` 2
      let y = hoveredBlockY * cellSize + cellSize `div` 2
      liftIO $ RL.drawRectangle x y cellSize cellSize RL.gray
      whenM (State.isPlacingSignal &&^ notM (Entity.hasSignal hoveredNode)) $ do
        Node.getNeighbours hoveredNode >>= \case
          [] -> return ()
          neighbour : _ ->
            renderSignal (Signal False neighbour, hovPos)

renderTracks :: System World ()
renderTracks = cmapM_ renderConnection

renderConnection :: (NodeType, Entity) -> System World ()
renderConnection (Empty, _) = return ()
renderConnection (DeadEnd neighbour, node) =
  void $ renderTrack node neighbour
renderConnection (Through n n', node) = do
  mapM_ (renderTrack node) [n, n']
renderConnection (Junction (n, n') neighbours, node) = do
  mapM_ (renderTrackMiddle node) neighbours
  mapM_ (renderTrack node) [n, n']

renderBuildMode :: System World ()
renderBuildMode = do
  Camera camera <- get global
  liftIO $ do
    let Vector2 (CFloat tx) (CFloat ty) = RL.camera2D'target camera
    let CFloat zoom = RL.camera2d'zoom camera
    let dx = fromEnum tx `div` cellSize - 1
    let dy = fromEnum ty `div` cellSize - 1
    h <- RL.getScreenHeight
    w <- RL.getScreenWidth
    let n = fromEnum (fromIntegral h / (cellSize * zoom)) + 1
    let m = fromEnum (fromIntegral w / (cellSize * zoom)) + 1
    mapM_ (renderHorizontalLine dx m) [dy .. n + dy]
    mapM_ (renderVerticalLine dy n) [dx .. m + dx]
  where
    renderHorizontalLine z m n = do
      RL.drawLineV
        (getScreenPosF $ V2 (fromIntegral z - 0.5) (fromIntegral n - 0.5))
        (getScreenPosF $ V2 (fromIntegral (m + z) + 0.5) (fromIntegral n - 0.5))
        RL.darkGray

    renderVerticalLine z m n = do
      RL.drawLineV
        (getScreenPosF $ V2 (fromIntegral n - 0.5) (fromIntegral z - 0.5))
        (getScreenPosF $ V2 (fromIntegral n - 0.5) (fromIntegral (m + z) + 0.5))
        RL.darkGray

renderTrackMiddle :: Entity -> Entity -> System World Entity
renderTrackMiddle node node' = do
  (GridPosition v1) <- Entity.getPosition node
  (GridPosition v2) <- Entity.getPosition node'
  let (Vector2 (CFloat v1x) (CFloat v1y)) = getScreenPos v1
  let (Vector2 (CFloat v2x) (CFloat v2y)) = getScreenPos v2
  let v1MiddlePos = Vector2 (CFloat ((v1x + v2x) / 4 + v1x / 2)) (CFloat ((v1y + v2y) / 4 + v1y / 2))
  let v2MiddlePos = Vector2 (CFloat ((v1x + v2x) / 4 + v2x / 2)) (CFloat ((v1y + v2y) / 4 + v2y / 2))
  liftIO $ do
    RL.drawLineEx v1MiddlePos v2MiddlePos 3 RL.white
    return node

renderTrack :: Entity -> Entity -> System World Entity
renderTrack node node' = do
  isBusy <- Entity.isBusy node
  isBusy' <- Entity.isBusy node'
  let color = if isBusy && isBusy' then RL.red else RL.white
  (GridPosition v1) <- Entity.getPosition node
  (GridPosition v2) <- Entity.getPosition node'
  let v1ScreenPos@(Vector2 (CFloat v1x) (CFloat v1y)) = getScreenPos v1
  let (Vector2 (CFloat v2x) (CFloat v2y)) = getScreenPos v2
  let v2MiddlePos = Vector2 (CFloat ((v1x + v2x) / 4 + v2x / 2)) (CFloat ((v1y + v2y) / 4 + v2y / 2))
  liftIO $ do
    RL.drawLineEx v1ScreenPos v2MiddlePos 3 color
    RL.drawCircleV v1ScreenPos 1.5 color
    return node

renderReachableNodes :: System World ()
renderReachableNodes =
  cmapM_ $ \(Reachable, Node, pos) -> renderNode pos

renderSignals :: System World ()
renderSignals = cmapM_ renderSignal

renderSignal :: (Signal, GridPosition) -> System World ()
renderSignal (Signal green towards, GridPosition signalPos) = do
  GridPosition towardsPos <- Entity.getPosition towards
  let dir = fmap fromIntegral signalPos ^-^ fmap fromIntegral towardsPos
  let angle = Linear.unangle dir
  let color = if green then RL.green else RL.red
  liftIO $
    RL.drawTriangle
      (getScreenPosF $ Linear.angle (angle + 4 * pi / 3) ^* 0.4 ^+^ toFloatVector signalPos)
      (getScreenPosF $ Linear.angle (angle + 2 * pi / 3) ^* 0.4 ^+^ toFloatVector signalPos)
      (getScreenPosF $ Linear.angle angle ^* 0.4 ^+^ toFloatVector signalPos)
      color