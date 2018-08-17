{-# LANGUAGE NoImplicitPrelude #-}

module QuadTree.QuadTree where

import           BasePrelude hiding (insert)
import           Control.Lens (ix, (%~))
import           Data.Array.Unboxed
import           Data.Map.Strict (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Linear.V2
import           Linear (quadrance)


data QuadTree k x = QuadTree
  { qtMap     :: {-# UNPACK #-} !(Map k (V2 x))
  , qtEntSize :: {-# UNPACK #-} !(Map k x)
  , qtSpace   :: {-# UNPACK #-} !(Array (Int, Int) (Set k))
  , qtBiggest :: {-# UNPACK #-} !x
  , qtSize    :: {-# UNPACK #-} !(V2 x)
  }


mkQuadTree :: (Num x, Ord k) => (Int, Int) -> V2 x -> QuadTree k x
mkQuadTree (aw, ah) =
  QuadTree mempty
           mempty
           (listArray ((0, 0), (aw - 1, ah - 1)) $ repeat mempty)
           0


getLoc :: Ord k => QuadTree k x -> k -> Maybe (V2 x)
getLoc QuadTree{..} k = M.lookup k qtMap

getLocSize :: Ord k => QuadTree k x -> k -> Maybe (V2 x, x)
getLocSize qt k = (,) <$> getLoc qt k <*> getSize qt k


setSize :: (Ord k, Ord x) => QuadTree k x -> k -> x -> QuadTree k x
setSize qt@QuadTree{..} k x =
  qt { qtEntSize = M.insert k x qtEntSize
     , qtBiggest = max qtBiggest x
     }


getSize :: Ord k => QuadTree k x -> k -> Maybe x
getSize QuadTree{..} k = M.lookup k qtEntSize


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
   in (floor $ x / sx, floor $ y / sy)


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


removeSize :: (RealFrac x, Ord k) => QuadTree k x -> k -> QuadTree k x
removeSize qt@QuadTree{..} k = qt { qtEntSize = M.delete k qtEntSize }


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


zones :: QuadTree k x -> [(Int, Int)]
zones = indices . qtSpace


inZone :: (Ord k, RealFrac x) => QuadTree k x -> (Int, Int) -> [(k, V2 x)]
inZone qt@QuadTree{..} i =
  mapMaybe (\a -> sequenceA (a, getLoc qt a)) . S.toList $ qtSpace ! i


pointInRect :: Ord x => V2 x -> (V2 x, V2 x) -> Bool
pointInRect (V2 x y) (V2 lx ly, V2 hx hy) =
  and [ lx <= x
      , x  <  hx
      , ly <= y
      , y  <  hy
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


rectRectIntersect :: (Ord x, Num x) => (V2 x, V2 x) -> (V2 x, V2 x) -> Bool
rectRectIntersect rect1@(tl1@(V2 l1 t1), br1@(V2 r1 b1))
                  rect2@(tl2@(V2 l2 t2), br2@(V2 r2 b2)) =
  or [ pointInRect tl2 rect1
     , pointInRect tr2 rect1
     , pointInRect bl2 rect1
     , pointInRect br2 rect1
     , pointInRect tl1 rect2
     , pointInRect tr1 rect2
     , pointInRect bl1 rect2
     , pointInRect br1 rect2
     ]
  where
    tr1 = V2 r1 t1
    bl1 = V2 l1 b1
    tr2 = V2 r2 t2
    bl2 = V2 l2 b2


inRange :: (Ord k, RealFrac x) => QuadTree k x -> V2 x -> x -> [(k, V2 x)]
inRange qt@QuadTree{..} p rd =
  let allTiles  = fmap (\i -> (i, tile qt i)) $ indices qtSpace
      nearTiles = filter (rectCircleIntersect p rd . snd) allTiles
   in filter (flip (pointInCircle p) rd . snd)
    . mapMaybe (\a -> sequenceA (a, getLoc qt a))
    . S.toList
    $ foldMap ((qtSpace !) . fst) nearTiles


inRect :: (Ord k, RealFrac x) => QuadTree k x -> (V2 x, V2 x) -> [(k, V2 x)]
inRect qt@QuadTree{..} rect =
  let allTiles  = fmap (\i -> (i, tile qt i)) $ indices qtSpace
      nearTiles = filter (rectRectIntersect rect . snd) allTiles
   in filter (flip pointInRect rect . snd)
    . mapMaybe (\a -> sequenceA (a, getLoc qt a))
    . S.toList
    $ foldMap ((qtSpace !) . fst) nearTiles

