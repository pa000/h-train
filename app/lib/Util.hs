{-# OPTIONS -Wall #-}

module Util (multiplyVector, (|*), foldrM, foldrM_) where

import Control.Monad (void)
import Foreign.C (CFloat (CFloat))
import Linear (V2)
import Linear.V2 (V2 (..))
import Raylib.Types (Vector2 (..))

multiplyVector :: V2 Int -> Float -> Vector2
multiplyVector (V2 x y) f =
  Vector2
    (CFloat (fromIntegral x * f))
    (CFloat (fromIntegral y * f))

(|*) :: V2 Int -> Float -> Vector2
(|*) = multiplyVector

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f d = foldr ((=<<) . f) (return d)

foldrM_ :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m ()
foldrM_ f d xs = void $ foldrM f d xs