{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Types
  ( Inventory (..),
    GoPast (..),
    Seed (..),
    Train (..),
    Position (..),
    Speed (..),
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
    Scene (..),
    Station (..),
    Timer (..),
    Action (..),
    initWorld,
    World,
  )
where

import Apecs
import Apecs.Experimental.Reactive
import Data.Ix
import Linear.V2
import Random (MRGen)
import qualified Raylib.Types as RL hiding (Camera)

newtype GridPosition = GridPosition (V2 Int)
  deriving (Eq, Ord, Ix, Show)

instance Component GridPosition where
  type Storage GridPosition = Reactive (OrdMap GridPosition) (Map GridPosition)

data Node = Node

instance Component Node where
  type Storage Node = Map Node

data NodeType
  = Empty
  | DeadEnd !Entity
  | Through !Entity !Entity
  | Junction !(Entity, Entity) ![Entity]

instance Component NodeType where
  type Storage NodeType = Map NodeType

data Train = Train

instance Component Train where
  type Storage Train = Map Train

data Position = Position !Entity !Entity !Float

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

newtype CoupledTo = CoupledTo Entity

instance Component CoupledTo where
  type Storage CoupledTo = Map CoupledTo

data State = State
  { buildingMode :: Bool,
    destructionMode :: Bool,
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

data Signal = Signal !Bool !Entity

instance Component Signal where
  type Storage Signal = Map Signal

data Station = Station

instance Component Station where
  type Storage Station = Map Station

data Scene = MainMenu | Pause | Game

instance Component Scene where
  type Storage Scene = Unique Scene

data Action = StartGame | ToggleBuildMode | ToggleBuildSignal | ToggleDestructionMode

instance Component Action where
  type Storage Action = Unique Action

newtype Timer = Timer Float

instance Component Timer where
  type Storage Timer = Map Timer

newtype Seed = Seed MRGen

instance Component Seed where
  type Storage Seed = Unique Seed

newtype GoPast = GoPast Entity

instance Component GoPast where
  type Storage GoPast = Map GoPast

data Item = ITrack | ITrain | IPassenger

newtype Inventory = Inventory [(Item, Int)]

instance Component Inventory where
  type Storage Inventory = Map Inventory

makeWorld
  "World"
  [ ''CoupledTo,
    ''Position,
    ''Speed,
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
    ''Scene,
    ''Camera,
    ''Timer,
    ''Station,
    ''Action,
    ''Seed,
    ''GoPast,
    ''Inventory
  ]