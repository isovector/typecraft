{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Ecstasy
import Game.Sequoia
import Data.Functor.Identity (Identity)


type Game = SystemT EntWorld Identity

data Nav
  = Goal V2

data UnitType
  = Unit

data Player = Player
  { pColor :: Color
  }


type Flag f = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = World
  { pos      :: Field f V2
  , pathing  :: Field f Nav
  , speed    :: Field f Double
  , selected :: Flag f
  , unitType :: Field f UnitType
  , owner    :: Field f Player
  }
  deriving (Generic)


type World = EntWorld 'WorldOf

