{-# LANGUAGE NoImplicitPrelude #-}

module Overture
  ( module Types
  , module BasePrelude
  , module Game.Sequoia
  , module Game.Sequoia.Utils
  , module Control.Lens
  , module Overture
  , module Data.Ecstasy
  , module Linear
  , module Game.Sequoia.Window
  ) where

import Linear (norm, normalize, (*^), (^*))
import Control.Lens hiding (without)
import Types
import BasePrelude hiding (group, rotate, lazy, index, uncons)
import Game.Sequoia
import Game.Sequoia.Utils (showTrace)
import Game.Sequoia.Window (MouseButton (..))
import Data.Ecstasy
import Data.Functor.Identity (Identity (..))


getFlag
    :: (EntWorld 'FieldOf -> Maybe ())
    -> QueryT EntWorld Identity Bool
getFlag = fmap (maybe False (const True)) . getMaybe


runGame
    :: SystemState EntWorld
    -> SystemT EntWorld Identity a
    -> (SystemState EntWorld, a)
runGame = (runIdentity .) . yieldSystemT


buttonLeft :: MouseButton
buttonLeft = ButtonExtra 0


buttonRight :: MouseButton
buttonRight = ButtonExtra 2


fi :: (Num b, Integral a) => a -> b
fi = fromIntegral


toV2 :: (Int, Int) -> V2
toV2 = uncurry V2 . (fi *** fi)

