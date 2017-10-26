{-# LANGUAGE NoImplicitPrelude #-}

module Collision where

import Constants
import Data.QuadTree
import Control.Lens
import Types
import Data.Functor.Foldable
import Utils
import Game.Sequoia.Color (black, red)


buildQuadTree :: (Int, Int) -> [Building] -> QuadTree Bool
buildQuadTree size = cata alg
  where
    alg Nil = makeTree size False
    alg (Cons b t)
      = ($ t)
      . appEndo
      . foldMap (Endo . (.~ True) . atLocation)
      $ [ (x + dx, y + dy)
        | dx <- [0 .. (b ^. bPrototype . upWidth  `div` tileWidth ) - 1]
        , dy <- [0 .. (b ^. bPrototype . upHeight `div` tileHeight) - 1]
        , let (x, y) = (view _x &&& view _y)
                     . gridPos
                     $ b ^. bStats . usPos
        ]

isLegit :: (Int, Int, Int, Int) -> Bool
isLegit (x, y, x', y') = x <= x' && y <= y' && x >= 0 && y >= 0


getCollisionGraph :: QuadTree Bool -> [Tile Bool]
getCollisionGraph = filter (isLegit . snd)
                  . filterTiles not
                  . tile


debugDrawQuad :: QuadTree Bool -> [Form]
debugDrawQuad = fmap (debugDraw . snd)
              . getCollisionGraph
  where
    debugDraw r@(x, y, x', y')
      = move (centerOf r)
      . traced' black
      $ rect (fi $ (x' - x + 1) * tileWidth) (fi $ (y' - y + 1) * tileHeight)

centerOf :: Region -> V2
centerOf (x, y, x', y') =
  V2 ((fi (x' - x + 1) / 2 + fi x) * fi tileWidth) ((fi (y' - y + 1) / 2 + fi y) * fi tileHeight)

debugDrawConnectivity :: QuadTree Bool -> [Form]
debugDrawConnectivity = fmap (uncurry debugDraw)
                      . adjacentTiles
                      . getCollisionGraph
  where
    debugDraw (_, r1) (_, r2)
      = traced (defaultLine { lineColor = red, lineWidth = 2 })
      $ path [centerOf r1, centerOf r2]

adjacentTiles :: [Tile a] -> [(Tile a, Tile a)]
adjacentTiles ts = do
  a <- ts
  b <- ts
  guard $ snd a `adjacent` snd b
  pure (a, b)


adjacent :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
adjacent (x1, y1, x1', y1') (x2, y2, x2', y2') =
    (rowAdjacent || rowContained) && (colAdjacent || colContained)
  where
    rowAdjacent = y1 == y2' + 1 || y2 == y1' + 1
    colAdjacent = x1 == x2' + 1 || x2 == x1' + 1
    rowContained = (y1 >= y2 && y1' <= y2') || (y2 >= y1 && y2' <= y1')
    colContained = (x1 >= x2 && x1' <= x2') || (x2 >= x1 && x2' <= x1')

