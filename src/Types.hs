{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Types
  ( module Types
  , await
  , resume
  , Await (..)
  , Key (..)
  ) where

import Control.Lens (makeLenses, makePrisms)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State.Strict
import Data.Ecstasy
import Game.Sequoia
import Game.Sequoia.Keyboard
import Game.Sequoia.Window (MouseButton (..))
import QuadTree.QuadTree (QuadTree)


data Order
  = Ordered Action
  | Implied Action


data NavMesh = NavMesh
  { nmTest :: (Int, Int) -> Bool
  , nmFind :: (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
  }


data Map = Map
  { mapGeometry  :: Int -> Int -> Maybe Form
  , mapDoodads   :: Int -> Int -> Maybe Form
  , mapNavMesh   :: NavMesh
  , mapWidth     :: Int
  , mapHeight    :: Int
  }


data Mouse = Mouse
  { mDown    :: !(MouseButton -> Bool)
  , mUp      :: !(MouseButton -> Bool)
  , mPress   :: !(MouseButton -> Bool)
  , mUnpress :: !(MouseButton -> Bool)
  , mPos     :: !V2
  }

data Keyboard = Keyboard
  { kPress   :: !(Key -> Bool)
  , kUnpress :: !(Key -> Bool)
  , kPresses :: ![Key]
  , kDown    :: !(Key -> Bool)
  , kUp      :: !(Key -> Bool)
  }


data LocalState = LocalState
  { _lsSelBox     :: !(Maybe V2)
  , _lsPlayer     :: !Player
  , _lsTasks      :: ![Task ()]
  , _lsTargetType :: !(Maybe (TargetType (Using Ability)))
  , _lsDynamic    :: !(QuadTree Ent Double)
  , _lsMap        :: !Map
  }


data Limit a = Limit
  { _limVal :: !a
  , _limMax :: !a
  }
  deriving (Eq, Ord, Show)


data Attack = Attack
  { _aCooldown  :: !(Limit Time)
  , _aRange     :: !Double
  , _aTask      :: !(Ent -> Target -> Task ())
  }

type Underlying = State LocalState
type Query = QueryT EntWorld Underlying


type Game = SystemT EntWorld Underlying
type Task = Coroutine (Await Time) Game


type Proto = EntWorld 'FieldOf
type Ability = Ent -> Target -> Task ()
type DamageHandler = V2 -> Target -> Game ()

data Action
  = MoveAction V2
  | AttackAction Ent
  -- | AttackMove V2
  -- | FollowAction Ent
  | StopAction
  | AbilityAction AbilityData


data AbilityData = AbilityData
  { _acName   :: !String
  , _acHotkey :: !(Maybe Key)
  , _acTType  :: !(TargetType ())
  , _acTask   :: !Ability
  }


data TargetType a
  = TargetTypeUnit    { unTargetType :: !a }
  | TargetTypeGround  { unTargetType :: !a }
  | TargetTypeInstant { unTargetType :: !a }
  deriving (Functor)


data Using a = Using
  { _usingEnt  :: !Ent
  , _usingWhat :: !a
  }
  deriving (Functor)

data Target
  = TargetUnit Ent
  | TargetGround V2

data UnitType
  = Unit
  | Missile
  | Building
  deriving (Eq, Ord, Show, Bounded, Enum)

data MovementType
  = GroundMovement
  | AirMovement
  deriving (Eq, Ord, Show, Bounded, Enum)

data Player = Player
  { pColor :: !Color
  }
  deriving (Eq)


type Flag f = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = World
  { pos      :: !(Component f 'Virtual V2)
  , gfx      :: !(Field f Form)
  , hp       :: !(Field f (Limit Int))
  , acqRange :: !(Field f Double)
  , pathing  :: !(Field f [V2])
  , speed    :: !(Field f Double)
  , entSize  :: !(Field f Double)
  , gridSize :: !(Field f (Int, Int))
  , selected :: !(Flag f)
  , unitType :: !(Field f UnitType)
  , moveType :: !(Field f MovementType)
  , owner    :: !(Field f Player)
  , attack   :: !(Field f Attack)
  , target   :: !(Field f Target)
  , order    :: !(Field f Order)
  , actions  :: !(Field f [Action])
  , isAlive  :: !(Field f ())
  }
  deriving (Generic)


type World = EntWorld ('WorldOf Underlying)


makeLenses ''LocalState
makeLenses ''Attack
makeLenses ''Limit

makePrisms ''UnitType

