{-# LANGUAGE NoImplicitPrelude #-}

module QuadTree.QuadTree where

import           BasePrelude hiding (insert)
import           Control.Lens (ix, (%~))
import           Data.Array
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Linear.V2
import           Linear (quadrance)


data QuadTree k x = QuadTree
  { qtMap   :: Map k (V2 x)
  , qtSpace :: Array (Int, Int) (Set k)
  , qtSize  :: V2 x
  }


mkQuadTree :: Ord k => (Int, Int) -> V2 x -> QuadTree k x
mkQuadTree (aw, ah) size =
  QuadTree mempty (listArray ((0, 0), (aw - 1, ah - 1)) $ repeat mempty) size


getStride :: RealFrac x => QuadTree k x -> (x, x)
getStride QuadTree{..} =
  let (m, n) = snd $ bounds qtSpace
      (V2 w h) = qtSize
      sx = w / fromIntegral m
      sy = h / fromIntegral n
   in (sx, sy)


qtPoint :: RealFrac x => QuadTree k x -> V2 x -> (Int, Int)
qtPoint qt (V2 x y) =
  let (sx, sy) = getStride qt
   in (round $ x / sx, round $ y / sy)


remove :: (RealFrac x, Ord k) => QuadTree k x -> k -> QuadTree k x
remove qt@QuadTree{..} k =
  case M.lookup k qtMap of
    Just p ->
      let pt = qtPoint qt p
       in qt
          { qtMap   = M.delete k qtMap
          , qtSpace = qtSpace & ix pt %~ S.delete k
          }
    Nothing -> qt


insert :: (RealFrac x, Ord k) => QuadTree k x -> k -> V2 x -> QuadTree k x
insert qt@QuadTree{..} k p =
  let pt = qtPoint qt p
   in qt
      { qtMap   = M.insert k p qtMap
      , qtSpace = qtSpace & ix pt %~ S.insert k
      }


move :: (RealFrac x, Ord k) => QuadTree k x -> k -> V2 x -> QuadTree k x
move qt k = insert (remove qt k) k


tile :: RealFrac x => QuadTree k x -> (Int, Int) -> (V2 x, V2 x)
tile qt (x, y) =
  let (sx, sy) = getStride qt
      x' = fromIntegral x
      y' = fromIntegral y
   in ( V2 (sx * x')
           (sy * y')
      , V2 (sx * (x' + 1))
           (sy * (y' + 1))
      )


pointInRect :: Ord x => V2 x -> (V2 x, V2 x) -> Bool
pointInRect (V2 x y) (V2 lx ly, V2 hx hy) =
  and [ lx <= x
      , x < hx
      , ly <= y
      , y < hy
      ]


pointInCircle :: (Ord x, Num x) => V2 x -> V2 x -> x -> Bool
pointInCircle p1 p2 r = quadrance (p1 - p2) < r * r


rectCircleIntersect :: (Ord x, Num x) => V2 x -> x -> (V2 x, V2 x) -> Bool
rectCircleIntersect p rd rect@(tl@(V2 l t), br@(V2 r b)) =
  or [ pointInRect p rect
     , pointInCircle tl p rd
     , pointInCircle br p rd
     , pointInCircle tr p rd
     , pointInCircle bl p rd
     ]
  where
    tr = V2 r t
    bl = V2 l b


inRange :: (Ord k, RealFrac x) => QuadTree k x -> V2 x -> x -> [k]
inRange qt@QuadTree{..} p rd =
  let allTiles  = fmap (\i -> (i, tile qt i)) $ indices qtSpace
      nearTiles = filter (rectCircleIntersect p rd . snd) allTiles
   in S.toList $ foldMap ((qtSpace !) . fst) nearTiles

