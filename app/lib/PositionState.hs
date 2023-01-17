{-# OPTIONS -Wall #-}

module PositionState where

import Apecs
import Control.Monad.State
import qualified Control.Monad.State as State
import Types

type PositionState a = (StateT Position (SystemT World IO) a)

move :: Float -> PositionState ()
move step = do
  _ <- State.get
  return ()