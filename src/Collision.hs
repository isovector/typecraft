{-# LANGUAGE NoImplicitPrelude #-}

module Collision where

import Data.Graph.AStar
import           Constants
import           Control.Lens
import           Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.HashSet as HS
import           Data.QuadTree
import           Game.Sequoia.Color (black, red)
import           Types
import           Utils
import Linear.Metric (quadrance)


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
                      . concatMap sequenceA
                      . M.toList
                      . adjacentTiles
                      . getCollisionGraph
  where
    debugDraw (_, r1) (_, r2)
      = traced (defaultLine { lineColor = red, lineWidth = 2 })
      $ path [centerOf r1, centerOf r2]

adjacentTiles :: Ord a => [Tile a] -> M.Map (Tile a) [Tile a]
adjacentTiles ts
  = M.fromListWith (++)
  $ do
    a <- ts
    b <- ts
    guard $ snd a `adjacent` snd b
    pure (a, [b])

adjacent :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
adjacent (x1, y1, x1', y1') (x2, y2, x2', y2') =
    (rowAdjacent || rowContained) && (colAdjacent || colContained)
  where
    rowAdjacent = y1 == y2' + 1 || y2 == y1' + 1
    colAdjacent = x1 == x2' + 1 || x2 == x1' + 1
    rowContained = (y1 >= y2 && y1' <= y2') || (y2 >= y1 && y2' <= y1')
    colContained = (x1 >= x2 && x1' <= x2') || (x2 >= x1 && x2' <= x1')

getTile :: QuadTree a -> V2 -> Maybe (Tile a)
getTile qt v2 = listToMaybe . filter (inRegion loc . snd) $ tile qt
  where
    loc = (view _x &&& view _y) $ gridPos v2

pathfind :: QuadTree Bool -> V2 -> V2 -> Maybe [Tile Bool]
pathfind qt srcv2 dstv2 = do
  let at = adjacentTiles $ tile qt
  src <- getTile qt srcv2
  dst <- getTile qt dstv2
  let dist = ((quadrance .) . on (-) (centerOf . snd))
  aStar (HS.fromList . (M.!) at)
        dist
        (dist dst)
        (== dst)
        src


-- :: (Hashable a, Ord a, Ord c, Num c)
-- => (a -> HashSet a)
-- -> (a -> a -> c)
-- -> (a -> c)
-- -> (a -> Bool)
-- -> a
-- -> Maybe [a]

