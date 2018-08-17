{-# LANGUAGE DeriveDataTypeable #-}

module Behavior where

import Data.Data
import Overture

data MoveCmd = MoveCmd [V2]

instance Data MoveCmd

instance IsLocationCommand MoveCmd
instance IsCommand MoveCmd

