{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

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


data AABB = AABB
  { aabbPos  :: V2
  , aabbSize :: V2
  } deriving (Eq, Show, Ord)


inAABB :: AABB -> V2 -> Bool
inAABB AABB {..} pos =
  let V2 x y = pos - aabbPos
   in x >= 0 && x < view _x aabbSize
   && y >= 0 && y < view _y aabbSize


data Panel a = Panel
  { panelAABB   :: AABB
  , panelAction :: a
  , panelForm   :: Form
  } deriving (Eq, Show, Functor)


getPanelAction :: [Panel a] -> V2 -> Maybe a
getPanelAction ps pos = fmap panelAction
                      . listToMaybe
                      $ filter (flip inAABB pos . panelAABB) ps

