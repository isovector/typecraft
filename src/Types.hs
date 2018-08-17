{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DeriveTraversable        #-}
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
import Data.Data
import Data.Ecstasy
import Game.Sequoia
import Game.Sequoia.Keyboard
import Game.Sequoia.Window (MouseButton (..))
import QuadTree.QuadTree (QuadTree)



data NavMesh = NavMesh
  { nmIsOpen :: (Int, Int) -> Bool
  , nmFind   :: (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
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
  , _lsDynamic    :: !(QuadTree Ent Double)
  , _lsMap        :: !Map
  }


data Limit a = Limit
  { _limVal :: !a
  , _limMax :: !a
  }
  deriving (Eq, Ord, Show)


data AttackData = AttackData
  { _aCooldown :: Limit Time
  , _aRange    :: Double
  , _aClass    :: [Maybe Classification]
  , _aTask     :: Ent -> Target -> Task ()
  }

type Underlying = State LocalState
type Query = QueryT EntWorld Underlying


type Game = SystemT EntWorld Underlying
type Task = Coroutine (Await Time) Game


type Proto = EntWorld 'FieldOf
type Ability = Ent -> Target -> Task ()
type DamageHandler = V2 -> Target -> Game ()




data Target
  = TargetUnit Ent
  | TargetGround V2

data UnitType
  = Unit
  | Missile
  | Building
  deriving (Eq, Ord, Show, Bounded, Enum)

data Classification
  = GroundUnit
  | AirUnit
  | BuildingUnit
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
  { pos            :: !(Component f 'Virtual V2)
  , gfx            :: !(Field f Form)
  , hp             :: !(Field f (Limit Int))
  , acqRange       :: !(Field f Double)
  , speed          :: !(Field f Double)
  , entSize        :: !(Component f 'Virtual Double)
  , gridSize       :: !(Field f (Int, Int))
  , selected       :: !(Flag f)
  , unitType       :: !(Field f UnitType)
  , moveType       :: !(Field f MovementType)
  , owner          :: !(Field f Player)
  , attacks        :: !(Field f [AttackData])
  , target         :: !(Field f Target)
  , isAlive        :: !(Field f ())
  , classification :: !(Field f Classification)
  , command        :: !(Field f Command)
  }
  deriving (Generic)


type World = EntWorld ('WorldOf Underlying)

data Attempt a
  = Attempted
  | Failure String
  | Success a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Attempt where
  pure = Success
  Success f <*> Success a = Success $ f a
  Failure e1 <*> Failure e2 = Failure $ e1 ++ "\n" ++ e2
  Failure err <*> _ = Failure err
  _ <*> Failure err = Failure err
  Attempted <*> _ = Attempted
  _ <*> Attempted = Attempted


class IsLocationCommand a where
  fromLocation :: Ent -> V2 -> Game (Attempt a)

class IsInstantCommand a where
  fromInstant :: Ent -> Game (Attempt a)

class IsUnitCommand a where
  fromUnit :: Ent -> Ent -> Game (Attempt a)


class IsCommand a where
  pumpCommand :: Time -> Ent -> a -> Game (Maybe a)

data SomeCommand c where
  SomeCommand
      :: (Data a, IsCommand a, c a)
      => a
      -> SomeCommand c

type Command = SomeCommand IsAnyCommand

class IsAnyCommand a
instance IsAnyCommand a



makeLenses ''LocalState
makeLenses ''AttackData
makeLenses ''Limit

makePrisms ''UnitType

