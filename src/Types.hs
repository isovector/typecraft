{-# LANGUAGE DeriveFunctor                #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE RankNTypes                   #-}
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

import Data.Ecstasy
import Control.Lens
import BasePrelude hiding (rotate, group, (&), uncons, index, lazy, throw, Handler, runHandlers)
import Linear.Vector hiding (E (..))
import Game.Sequoia
import Game.Sequoia.Color (Color (..), red)
import Game.Sequoia.Keyboard (Key)


fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

data Space a
  = ScreenSpace a
  | WorldSpace a
  deriving (Eq, Ord, Show)


data Count = Count
  { _countCurrent :: Int
  , _countMax     :: Int
  }
  deriving (Eq, Ord, Show)

instance Num Count where
  fromInteger i = Count (fi i) (fi i)


data Player = Player
  { _pColor :: Color
  , _pId    :: Int
  }
  deriving (Eq, Ord, Show)


data Hotkey a = Hotkey
  { _hkKey    :: Maybe Key
  , _hkAction :: a
  }
  deriving (Eq, Ord, Show)


data Prototype = Prototype
  { _upGfx      :: Element
  , _upSize     :: V2
  , _upGridSize :: Maybe (Int, Int)
  , _upHp       :: Integer
  }
  deriving (Eq, Show)


data EntWorld f = World
  { spacePos     :: Component f 'Field (Space V2)
  , size         :: Component f 'Field V2
  , gridSize     :: Component f 'Field (Int, Int)
  , player       :: Component f 'Field Player
  , gfx          :: Component f 'Field Element
  , hp           :: Component f 'Field Count
  , synchronized :: Component f 'Field ()
  }
  deriving (Generic)


data AABB = AABB
  { _aabbPos  :: V2
  , _aabbSize :: V2
  } deriving (Eq, Show, Ord)


data InputState
  = NormalState
  | PlaceBuildingState Prototype
  | DebugVisPathingState V2
  deriving (Eq, Show)


data LocalState = LocalState
  { _lsInputState :: InputState
  , _lsCamera     :: V2
  , _lsPlayer     :: Int
  , _lsDebugVis   :: Form
  } deriving (Eq, Show)

makePrisms ''Space

pos :: EntWorld 'FieldOf -> Maybe V2
pos = preview (_Just . _WorldSpace) . spacePos

aabb :: Prism' (Space V2) V2 -> EntWorld 'FieldOf -> Maybe AABB
aabb l e = AABB <$> preview (_Just . l) (spacePos e)
                <*> size e



inAABB :: AABB -> V2 -> Bool
inAABB AABB {..} pos =
  let V2 x y = pos - _aabbPos
   in x >= 0 && x < view _x _aabbSize
   && y >= 0 && y < view _y _aabbSize


-- data Panel a = Panel
--   { _panelAABB   :: AABB
--   , _panelAction :: a
--   , _panelForm   :: Form
--   , _panelHotKey :: Maybe Key
--   } deriving (Eq, Show, Functor)


-- getPanelAction :: [Panel a] -> V2 -> Maybe a
-- getPanelAction ps pos = fmap _panelAction
--                       . listToMaybe
--                       $ filter (flip inAABB pos . _panelAABB) ps



-- data Command
--   = DoNothing
--   | PlaceBuilding UnitPrototype
--   | ConfirmBuilding UnitPrototype V2
--   | DebugVisStartPathing V2
--   | DebugVisPathing V2 V2
--   deriving (Eq, Show)


-- commandCenter :: UnitPrototype
-- commandCenter = UnitPrototype
--   { _upMaxHitpoints = 1500
--   , _upGfx          = colorCorrectedImage "assets/cc.png" (rgb 0 1 0)
--   , _upWidth        = 64
--   , _upHeight       = 48
--   }

-- nothing :: UnitPrototype
-- nothing = UnitPrototype
--   { _upMaxHitpoints = 1500
--   , _upGfx          = collage 0 0 []
--   , _upWidth        = 16
--   , _upHeight       = 16
--   }


