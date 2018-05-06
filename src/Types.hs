{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Types
  ( module Types
  , await
  , resume
  , Await (..)
  ) where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Lens (makeLenses, makePrisms)
import Game.Sequoia.Window (MouseButton (..))
import Data.Ecstasy
import Game.Sequoia
import Control.Monad.State


data Mouse = Mouse
  { mDown    :: MouseButton -> Bool
  , mUp      :: MouseButton -> Bool
  , mPress   :: MouseButton -> Bool
  , mUnpress :: MouseButton -> Bool
  , mPos     :: V2
  }


data LocalState = LocalState
  { _lsSelBox :: Maybe V2
  , _lsPlayer :: Player
  , _lsTasks  :: [Task ()]
  }


data Limit a = Limit
  { _limVal :: a
  , _limMax :: a
  }
  deriving (Eq, Ord, Show)


data Attack = Attack
  { _aCooldown  :: Limit Time
  , _aRange     :: Double
  , _aTask      :: Ent -> Target -> Task ()
  }

type Underlying = State LocalState
type Query = QueryT EntWorld Underlying


type Game = SystemT EntWorld Underlying
type Task = Coroutine (Await Time) Game

data Nav
  = Goal V2

data Target
  = TargetGround V2
  | TargetUnit Ent

data UnitType
  = Unit
  | Missile

data Player = Player
  { pColor :: Color
  }
  deriving (Eq)


type Flag f = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = World
  { pos      :: Field f V2
  , hp       :: Field f (Limit Int)
  , pathing  :: Field f Nav
  , speed    :: Field f Double
  , selected :: Flag f
  , unitType :: Field f UnitType
  , owner    :: Field f Player
  , attack   :: Field f Attack
  , target   :: Field f Target
  }
  deriving (Generic)


type World = EntWorld 'WorldOf


makeLenses ''LocalState
makeLenses ''Attack
makeLenses ''Limit

makePrisms ''UnitType

