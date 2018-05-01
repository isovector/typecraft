{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Ecstasy
import Game.Sequoia
import Data.Functor.Identity (Identity)


type Game = SystemT EntWorld Identity

data Nav
  = Goal V2

type Flag f = Component f 'Field ()

data EntWorld f = World
  { pos      :: Component f 'Field V2
  , pathing  :: Component f 'Field Nav
  , speed    :: Component f 'Field Double
  , selected :: Flag f
  }
  deriving (Generic)


type World = EntWorld 'WorldOf

