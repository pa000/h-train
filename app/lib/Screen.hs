{-# OPTIONS -Wall #-}

module Screen where

import Apecs
import Constants
import Foreign.C
import Linear
import qualified Raylib as RL
import Raylib.Types
import qualified Raylib.Types as RL
import Types

getScreenPos :: V2 Int -> Vector2
getScreenPos (V2 x y) =
  Vector2
    (CFloat . fromIntegral $ (x + 1) * cellSize)
    (CFloat . fromIntegral $ (y + 1) * cellSize)

getScreenPosF :: V2 Float -> Vector2
getScreenPosF (V2 x y) =
  Vector2 (CFloat (x + 1) * cellSize) (CFloat (y + 1) * cellSize)

isVisibleOnScreen :: GridPosition -> System World Bool
isVisibleOnScreen (GridPosition pos) = do
  Camera camera <- get global
  let Vector2 (CFloat tx) (CFloat ty) = RL.camera2D'target camera
  let CFloat zoom = RL.camera2d'zoom camera
  h <- liftIO RL.getScreenHeight
  w <- liftIO RL.getScreenWidth
  let (Vector2 (CFloat x) (CFloat y)) = getScreenPos pos
  return $
    - cellSize <= x - tx
      && x - tx <= fromIntegral w / zoom + cellSize
      && - cellSize <= y - ty
      && y - ty <= fromIntegral h / zoom + cellSize
