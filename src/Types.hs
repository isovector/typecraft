{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Types
  ( module Types
  , await
  , resume
  , Await (..)
  , Key (..)
  , Schema
  ) where

import qualified Algorithm.Search.JumpPoint as JP
import           Control.Lens (makeLenses, makePrisms)
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors
import           Control.Monad.State.Strict
import           Data.Ecstasy
import qualified Data.IntMap.Strict as IM
import           Data.Spriter.Types
import           Data.Typeable
import           Game.Sequoia
import           Game.Sequoia.Keyboard
import           Game.Sequoia.Window (MouseButton (..))
import           QuadTree.QuadTree (QuadTree)


type NavMesh = JP.JumpGrid


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
  { _lsSelBox      :: !(Maybe V2)
  , _lsPlayer      :: !Player
  , _lsTasks       :: !(IM.IntMap (Task ()))
  , _lsNewTasks    :: ![(Int, Task ())]
  , _lsTaskId      :: !Int
  , _lsDynamic     :: !(QuadTree Ent Double)
  , _lsMap         :: !Map
  , _lsNavMesh     :: !NavMesh
  , _lsCommandCont :: !(Maybe WaitingForCommand)
  }


data Limit a = Limit
  { _limVal :: !a
  , _limMax :: !a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Limit where
  pure a = Limit a a
  Limit fa fb <*> Limit a b = Limit (fa a) (fb b)


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


type Flag  f   = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = World
  { gfx            :: !(Field f Form)
  , acqRange       :: !(Field f Double)
  , speed          :: !(Field f Double)
  , entSize        :: !(Component f 'Virtual Double)
  , gridSize       :: !(Field f (Int, Int))
  , selected       :: !(Flag f)
  , unitType       :: !(Field f UnitType)
  , owner          :: !(Field f Player)
  , attacks        :: !(Field f [AttackData])
  , isAlive        :: !(Field f ())
  , classification :: !(Field f Classification)
  , commands       :: !(Field f [CommandWidget])
  , activePassives :: !(Field f [SomePassive])
  , isDepot        :: !(Flag f)

  , pos            :: !(Component f 'Virtual V2)
  , hp             :: !(Field f (Limit Int))
  , currentCommand :: !(Field f Command)
  , resourceSource :: !(Field f (Resource, Limit Int))
  , art            :: !(Field f Art)
  }
  deriving (Generic)

data Resource = Minerals
  deriving (Eq, Ord, Show)


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


class IsCommand a => IsLocationCommand a where
  fromLocation :: CommandParam a -> Ent -> V2 -> Game (Attempt a)

class IsCommand a => IsInstantCommand a where
  fromInstant :: CommandParam a -> Ent -> Game (Attempt a)

class IsCommand a => IsChannelCommand a where
  endChannel :: CommandParam a -> a -> Game ()

class IsCommand a => IsUnitCommand a where
  fromUnit :: CommandParam a -> Ent -> Ent -> Game (Attempt a)

class IsCommand a => IsPlacementCommand a where
  fromPlacement :: CommandParam a -> Ent -> (Int, Int) -> Game (Attempt a)


class Typeable a => IsCommand a where
  type CommandParam a
  type instance CommandParam a = ()
  pumpCommand
      :: Time
      -> Ent
      -> a
      -> Game (Maybe a)

data Command where
  SomeCommand
      :: IsCommand a
      => a
      -> Command

instance IsCommand Command where
  pumpCommand dt e (SomeCommand a) =
    fmap SomeCommand <$> pumpCommand dt e a


class    (IsChannelCommand a, IsInstantCommand a) => IsPassiveCommand a
instance (IsChannelCommand a, IsInstantCommand a) => IsPassiveCommand a


data SomePassive where
  SomePassive
      :: IsPassiveCommand a
      => CommandParam a
      -> a
      -> SomePassive


data Commanding f where
  LocationCommand
      :: IsLocationCommand a
      => f a V2
      -> Commanding f
  UnitCommand
      :: IsUnitCommand a
      => f a Ent
      -> Commanding f
  InstantCommand
      :: IsInstantCommand a
      => f a ()
      -> Commanding f
  PassiveCommand
      :: IsPassiveCommand a
      => f a ()
      -> Commanding f
  PlacementCommand
      :: IsPlacementCommand a
      => f a (Int, Int)
      -> Commanding f

instance Show (Commanding f) where
  show (LocationCommand _)  = "LocationCommand"
  show (UnitCommand _)      = "UnitCommand"
  show (InstantCommand _)   = "InstantCommand"
  show (PassiveCommand _)   = "PassiveCommand"
  show (PlacementCommand _) = "PlacementCommand"

data Proxy2 a b = Proxy2
  { getCommandParam :: CommandParam a
  }

data GameCont a b = GameCont
  { commandParam :: CommandParam a
  , unTag :: b -> Game ()
  }

type Commander         = Commanding Proxy2
type WaitingForCommand = Commanding GameCont


data CommandWidget = CommandWidget
  { cwName    :: String
  , cwCommand :: Commander
  , cwVisible :: Bool
  , cwHotkey  :: Maybe Key
  } deriving (Show)

data Art = Art
  { _aCanned :: CannedAnim
  , _aTime   :: Time
  , _aScale  :: Double
  } deriving (Eq)

data CannedAnim = CannedAnim
  { _aSchema    :: Schema
  , _aEntity    :: EntityName
  , _aAnim      :: AnimationName
  , _aSpeedMult :: Double
  , _aRepeat    :: Bool
  } deriving (Eq)



makeLenses ''LocalState
makeLenses ''AttackData
makeLenses ''Limit
makeLenses ''Art
makeLenses ''CannedAnim

makePrisms ''UnitType

