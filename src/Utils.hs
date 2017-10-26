{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Types
import Constants
import qualified Linear.V2


gridPos :: V2 -> Linear.V2.V2 Int
gridPos pos = floor @_ @Int <$> pos * V2 (1 / fi tileWidth)
                                         (1 / fi tileHeight)

alignToGrid :: V2 -> V2
alignToGrid = (* V2 (fi tileWidth) (fi tileHeight)) . fmap fi . gridPos

