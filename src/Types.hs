{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

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

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

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
  } deriving (Eq, Show, Functor)


getPanelAction :: [Panel a] -> V2 -> Maybe a
getPanelAction ps pos = fmap _panelAction
                      . listToMaybe
                      $ filter (flip inAABB pos . _panelAABB) ps

data InputState
  = NormalState
  | PlaceBuildingState UnitPrototype
  deriving (Eq, Show)


data State = State
  { _sLocalState :: LocalState
  , _sGameState  :: GameState
  } deriving (Eq, Show)


data LocalState = LocalState
  { _lsInputState :: InputState
  , _lsCamera     :: V2
  , _lsPlayer     :: Int
  } deriving (Eq, Show)

data GameState = GameState
  { _gsPlayers :: [Player]
  } deriving (Eq, Show)


data Player = Player
  { _pColor :: Color
  , _pOwned :: PlayerOwned
  } deriving (Eq, Show)


data PlayerOwned = PlayerOwned
  { _poBuildings :: [Building]
  } deriving (Eq, Show)


data Building = Building
  { _bPrototype :: UnitPrototype
  , _bStats     :: UnitStats
  } deriving (Eq, Show)


data UnitPrototype = UnitPrototype
  { _upMaxHitpoints :: Int
  , _upGfx          :: Element
  , _upWidth        :: Int
  , _upHeight       :: Int
  } deriving (Eq, Show)

data UnitStats = UnitStats
  { _usHP  :: Int
  , _usPos :: V2
  } deriving (Eq, Show, Ord)


commandCenter :: UnitPrototype
commandCenter = UnitPrototype
  { _upMaxHitpoints = 1500
  , _upGfx          = colorCorrectedImage "assets/cc.png" (rgb 0 1 0)
  , _upWidth        = 64
  , _upHeight       = 48
  }


makeLenses ''Player
makeLenses ''PlayerOwned
makeLenses ''Building
makeLenses ''UnitPrototype
makeLenses ''UnitStats
makePrisms ''InputState
makePrisms ''LocalState
makePrisms ''GameState
makePrisms ''State

