{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
    Clicked (..),
    Selected (..),
    Hovered (..),
    Switch (..),
    ConnectedTo (..),
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

newtype ConnectedTo = ConnectedTo [GridPosition]

data Train = Train

instance Component Train where
  type Storage Train = Map Train

instance Component ConnectedTo where
  type Storage ConnectedTo = Map ConnectedTo

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

data State = State
  { buildingMode :: Bool
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

newtype Switch = Switch (Entity, Entity)

instance Component Switch where
  type Storage Switch = Unique Switch

makeWorld
  "World"
  [ ''VehicleType,
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
    ''ConnectedTo,
    ''Train,
    ''Hovered,
    ''Switch
  ]