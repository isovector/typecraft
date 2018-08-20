{-# LANGUAGE DeriveDataTypeable #-}

module Behavior where

import Overture

data MoveCmd = MoveCmd [V2]

instance IsLocationCommand MoveCmd
instance IsCommand MoveCmd

