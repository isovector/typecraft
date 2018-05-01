{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Types where

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
  }


data Limit a = Limit
  { _limVal :: a
  , _limMax :: a
  }
  deriving (Eq, Ord, Show)


data Attack = Attack
  { _aTarget    :: Maybe Target
  , _aCooldown  :: Limit Time
  , _aProjSpeed :: Double
  }

type Underlying = State LocalState
type Query = QueryT EntWorld Underlying


type Game = SystemT EntWorld Underlying

data Nav
  = Goal V2

data Target
  = TargetGround V2
  | TargetUnit Ent

data UnitType
  = Unit
  | Missile Target

data Player = Player
  { pColor :: Color
  }
  deriving (Eq)


type Flag f = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = World
  { pos      :: Field f V2
  , pathing  :: Field f Nav
  , speed    :: Field f Double
  , selected :: Flag f
  , unitType :: Field f UnitType
  , owner    :: Field f Player
  , attack   :: Field f Attack
  }
  deriving (Generic)


type World = EntWorld 'WorldOf


makeLenses ''LocalState
makeLenses ''Attack
makeLenses ''Limit

makePrisms ''UnitType

