{-# LANGUAGE DeriveDataTypeable           #-}
{-# LANGUAGE DeriveFunctor                #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE StandaloneDeriving           #-}
{-# LANGUAGE StrictData                   #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans         #-}

module Types
  ( module Types
  , module Linear.Vector
  , module BasePrelude
  , module Game.Sequoia
  , module Control.Lens
  ) where

import Control.Lens
import BasePrelude hiding (rotate, group, (&), uncons, index, lazy, throw, Handler, runHandlers)
import Linear.Vector hiding (E (..))
import Game.Sequoia
import Game.Sequoia.Color (Color (..), red)
import Game.Sequoia.Keyboard (Key)


fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

instance Data Element
instance Data Form
deriving instance Data Color

data AABB = AABB
  { _aabbPos  :: V2
  , _aabbSize :: V2
  } deriving (Eq, Show, Ord)


inAABB :: AABB -> V2 -> Bool
inAABB AABB {..} pos =
  let V2 x y = pos - _aabbPos
   in x >= 0 && x < view _x _aabbSize
   && y >= 0 && y < view _y _aabbSize


data Panel a = Panel
  { _panelAABB   :: AABB
  , _panelAction :: a
  , _panelForm   :: Form
  , _panelHotKey :: Maybe Key
  } deriving (Eq, Show, Functor)


getPanelAction :: [Panel a] -> V2 -> Maybe a
getPanelAction ps pos = fmap _panelAction
                      . listToMaybe
                      $ filter (flip inAABB pos . _panelAABB) ps

data InputState
  = NormalState
  | PlaceBuildingState UnitPrototype
  | DebugVisPathingState V2
  deriving (Eq, Show, Data)


data State = State
  { _sLocalState :: LocalState
  , _sGameState  :: GameState
  } deriving (Eq, Show, Data)

defState :: State
defState = State
  { _sLocalState = defLocalState
  , _sGameState = defGameState
  }

data LocalState = LocalState
  { _lsInputState :: InputState
  , _lsCamera     :: V2
  , _lsPlayer     :: Int
  , _lsDebugVis   :: Form
  } deriving (Eq, Show, Data)

defLocalState :: LocalState
defLocalState = LocalState
  { _lsInputState = NormalState
  , _lsCamera = V2 0 0
  , _lsPlayer = 0
  , _lsDebugVis = group []
  }

data GameState = GameState
  { _gsPlayers :: [Player]
  } deriving (Eq, Show, Data)

defGameState :: GameState
defGameState = GameState
  { _gsPlayers = [defPlayer]
  }


data Player = Player
  { _pColor :: Color
  , _pOwned :: PlayerOwned
  } deriving (Eq, Show, Data)

defPlayer :: Player
defPlayer = Player
  { _pColor = red
  , _pOwned = PlayerOwned []
  }


data PlayerOwned = PlayerOwned
  { _poBuildings :: [Building]
  } deriving (Eq, Show, Data)


data Building = Building
  { _bPrototype :: UnitPrototype
  , _bStats     :: UnitStats
  } deriving (Eq, Show, Data)


data UnitPrototype = UnitPrototype
  { _upMaxHitpoints :: Int
  , _upGfx          :: Element
  , _upWidth        :: Int
  , _upHeight       :: Int
  } deriving (Eq, Show, Data)

data UnitStats = UnitStats
  { _usHP  :: Int
  , _usPos :: V2
  } deriving (Eq, Show, Ord, Data)


prototypeToStats :: V2 -> UnitPrototype -> UnitStats
prototypeToStats pos pt = UnitStats
  { _usHP = _upMaxHitpoints pt
  , _usPos = pos
  }


data Command
  = DoNothing
  | PlaceBuilding UnitPrototype
  | ConfirmBuilding UnitPrototype V2
  | DebugVisStartPathing V2
  | DebugVisPathing V2 V2
  deriving (Eq, Show)


commandCenter :: UnitPrototype
commandCenter = UnitPrototype
  { _upMaxHitpoints = 1500
  , _upGfx          = colorCorrectedImage "assets/cc.png" (rgb 0 1 0)
  , _upWidth        = 64
  , _upHeight       = 48
  }

nothing :: UnitPrototype
nothing = UnitPrototype
  { _upMaxHitpoints = 1500
  , _upGfx          = collage 0 0 []
  , _upWidth        = 16
  , _upHeight       = 16
  }


makeLenses ''Player
makeLenses ''PlayerOwned
makeLenses ''Building
makeLenses ''UnitPrototype
makeLenses ''UnitStats
makePrisms ''InputState
makeLenses ''LocalState
makeLenses ''GameState
makeLenses ''State

