{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Types
  ( VehicleType (..),
    Train (..),
    Position (..),
    Speed (..),
    Sector (..),
    Busy (..),
    State (..),
    GridPosition (..),
    Reachable (..),
    Node (..),
    Direction,
    CoupledTo (..),
    Clicked (..),
    Selected (..),
    Hovered (..),
    Camera (..),
    NodeType (..),
    cellSize,
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

cellSize :: Num p => p
cellSize = 30

data VehicleType = MU | WithLoco | JustCars

instance Component VehicleType where
  type Storage VehicleType = Map VehicleType

newtype GridPosition = GridPosition (V2 Int)
  deriving (Eq, Ord, Ix, Show)

instance Component GridPosition where
  type Storage GridPosition = Reactive (OrdMap GridPosition) (Map GridPosition)

data Node = Node

instance Component Node where
  type Storage Node = Map Node

data NodeType
  = Empty
  | DeadEnd GridPosition
  | Through GridPosition GridPosition
  | Junction (GridPosition, GridPosition) [GridPosition]

instance Component NodeType where
  type Storage NodeType = Map NodeType

data Train = Train

instance Component Train where
  type Storage Train = Map Train

data Position = Position ![GridPosition] !Float

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

newtype Sector = Sector (Seq GridPosition)

instance Component Sector where
  type Storage Sector = Map Sector

newtype CoupledTo = CoupledTo Entity

instance Component CoupledTo where
  type Storage CoupledTo = Map CoupledTo

data State = State
  { buildingMode :: Bool,
    placingSemaphore :: Bool
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

makeWorld
  "World"
  [ ''VehicleType,
    ''CoupledTo,
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
    ''Camera
  ]