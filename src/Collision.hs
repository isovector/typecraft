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
debugDrawConnectivity = fmap (uncurry $ debugDrawLine red)
                      . concatMap sequenceA
                      . M.toList
                      . adjacentTiles
                      . getCollisionGraph


debugDrawLine :: Color -> Tile a -> Tile a -> Form
debugDrawLine c t1 t2 = debugDrawLines c [t1, t2]

debugDrawLines :: Color -> [Tile a] -> Form
debugDrawLines c
  = traced (defaultLine { lineColor = c, lineWidth = 2 })
  . path
  . fmap (centerOf . snd)

adjacentTiles :: [Tile Bool] -> M.Map (Tile Bool) [Tile Bool]
adjacentTiles ts
  = M.fromListWith (++)
  $ do
    a <- ts
    b <- ts
    guard . not $ fst a || fst b
    guard $ snd a `adjacent` snd b
    guard $ snd a /= snd b
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
  (src : ) <$>
    aStar (HS.fromList . (M.!) at)
          dist
          (dist dst)
          (== dst)
          src

