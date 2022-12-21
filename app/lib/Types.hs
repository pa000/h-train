{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types
  ( VehicleType (..),
    Position (..),
    Speed (..),
    Sector (..),
    OccupiedBy (..),
    Node (..),
    Board (..),
    State (..),
    cellSize,
    initWorld,
    World,
  )
where

import Apecs
import qualified Data.Map as Map
import Data.Sequence
import Linear.V2
import Raylib.Types (Vector2)
import qualified Raylib.Types as RL

cellSize :: Num p => p
cellSize = 40

data VehicleType = MU | Loco | Car

instance Component VehicleType where
  type Storage VehicleType = Map VehicleType

newtype Position = Position (Entity, Float)

instance Component Position where
  type Storage Position = Map Position

newtype Speed = Speed Float

instance Component Speed where
  type Storage Speed = Map Speed

newtype Coupled = Coupled (Seq Entity)

instance Component Coupled where
  type Storage Coupled = Map Coupled

newtype OccupiedBy = OccupiedBy Entity

instance Component OccupiedBy where
  type Storage OccupiedBy = Map OccupiedBy

data Sector = Sector (V2 Int) (Seq (V2 Int)) (V2 Int)

instance Component Sector where
  type Storage Sector = Map Sector

data Node
  = DeadEnd Entity
  | Through Entity
  | Semaphore Bool Entity Entity
  | Junction [Entity] Entity Entity

instance Component Node where
  type Storage Node = Map Node

newtype Board = Board (Map.Map (V2 Int) Node)

instance Component Board where
  type Storage Board = Global Board

instance Semigroup Board where
  Board b1 <> Board b2 = Board $ Map.union b1 b2

instance Monoid Board where
  mempty = Board Map.empty

data State = State
  { buildingMode :: Bool,
    selectedBlock :: Maybe (V2 Int)
  }

instance Component State where
  type Storage State = Unique State

makeWorld
  "World"
  [ ''VehicleType,
    ''Position,
    ''Speed,
    ''Coupled,
    ''Sector,
    ''OccupiedBy,
    ''Node,
    ''Board,
    ''State
  ]