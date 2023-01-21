{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Types
  ( Train (..),
    Position (..),
    Speed (..),
    Sector (..),
    Busy (..),
    State (..),
    GridPosition (..),
    Signal (..),
    Reachable (..),
    Node (..),
    Direction,
    CoupledTo (..),
    Clicked (..),
    Selected (..),
    Hovered (..),
    Camera (..),
    NodeType (..),
    initWorld,
    World,
  )
where

import Apecs
import Apecs.Experimental.Reactive
import Data.Ix
import Data.Sequence
import Linear.V2
import qualified Raylib.Types as RL

newtype GridPosition = GridPosition (V2 Int)
  deriving (Eq, Ord, Ix, Show)

instance Component GridPosition where
  type Storage GridPosition = Reactive (OrdMap GridPosition) (Map GridPosition)

data Node = Node

instance Component Node where
  type Storage Node = Map Node

data NodeType
  = Empty
  | DeadEnd Entity
  | Through Entity Entity
  | Junction (Entity, Entity) [Entity]

instance Component NodeType where
  type Storage NodeType = Map NodeType

data Train = Train

instance Component Train where
  type Storage Train = Map Train

data Position = Position ![Entity] !Float

instance Component Position where
  type Storage Position = Map Position

newtype Speed = Speed Float

instance Component Speed where
  type Storage Speed = Map Speed

data Reachable = Reachable

instance Component Reachable where
  type Storage Reachable = Map Reachable

data Busy = Busy

instance Component Busy where
  type Storage Busy = Map Busy

newtype Sector = Sector (Seq Entity)

instance Component Sector where
  type Storage Sector = Map Sector

newtype CoupledTo = CoupledTo Entity

instance Component CoupledTo where
  type Storage CoupledTo = Map CoupledTo

data State = State
  { buildingMode :: Bool,
    placingSignal :: Bool
  }

instance Component State where
  type Storage State = Unique State

type Direction = V2 Int

data Clicked = Clicked

instance Component Clicked where
  type Storage Clicked = Unique Clicked

data Selected = Selected

instance Component Selected where
  type Storage Selected = Unique Selected

data Hovered = Hovered

instance Component Hovered where
  type Storage Hovered = Unique Hovered

newtype Camera = Camera RL.Camera2D

instance Component Camera where
  type Storage Camera = Unique Camera

data Signal = Signal Bool Entity

instance Component Signal where
  type Storage Signal = Map Signal

makeWorld
  "World"
  [ ''CoupledTo,
    ''Position,
    ''Speed,
    ''Sector,
    ''Busy,
    ''State,
    ''Reachable,
    ''GridPosition,
    ''Node,
    ''Clicked,
    ''Selected,
    ''NodeType,
    ''Train,
    ''Hovered,
    ''Signal,
    ''Camera
  ]