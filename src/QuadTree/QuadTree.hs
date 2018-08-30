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


------------------------------------------------------------------------------
-- | Make a new 'QuadTree'
mkQuadTree :: (Num x, Ord k) => (Int, Int) -> V2 x -> QuadTree k x
mkQuadTree (aw, ah) =
  QuadTree mempty
           mempty
           (listArray ((0, 0), (aw - 1, ah - 1)) $ repeat mempty)
           0


okZone :: QuadTree k x -> (Int, Int) -> Bool
okZone QuadTree{..} (x, y) =
  let (m, n) = snd $ bounds qtSpace
   in and [ x >= 0
          , x < m
          , y >= 0
          , y < n
          ]


------------------------------------------------------------------------------
-- | Get the location of a key.
getLoc :: Ord k => QuadTree k x -> k -> Maybe (V2 x)
getLoc QuadTree{..} k = M.lookup k qtMap


------------------------------------------------------------------------------
-- | Get the location and size of a key.
getLocSize :: Ord k => QuadTree k x -> k -> Maybe (V2 x, x)
getLocSize qt k = (,) <$> getLoc qt k <*> getSize qt k


------------------------------------------------------------------------------
-- | Set the size of a key.
setSize :: (Ord k, Ord x) => QuadTree k x -> k -> x -> QuadTree k x
setSize qt@QuadTree{..} k x =
  qt { qtEntSize = M.insert k x qtEntSize
     , qtBiggest = max qtBiggest x
     }


------------------------------------------------------------------------------
-- | Get the size of a key.
getSize :: Ord k => QuadTree k x -> k -> Maybe x
getSize QuadTree{..} k = M.lookup k qtEntSize


------------------------------------------------------------------------------
-- | Get the size of a zone in the quadtree.
getStride :: RealFrac x => QuadTree k x -> (x, x)
getStride QuadTree{..} =
  let (m, n) = snd $ bounds qtSpace
      (V2 w h) = qtSize
      sx = w / fromIntegral (m + 1)
      sy = h / fromIntegral (n + 1)
   in (sx, sy)


------------------------------------------------------------------------------
-- | Get the size of a zone in the quadtree.
getExtraZones :: RealFrac x => QuadTree k x -> (Int, Int)
getExtraZones qt@QuadTree{..} =
  let (sx, sy) = getStride qt
      (m, n) = snd $ bounds qtSpace
   in ( min m . max 0 . ceiling $ qtBiggest / sx
      , min n . max 0 . ceiling $ qtBiggest / sy
      )


------------------------------------------------------------------------------
-- | Get the zone a given location falls into.
locToZone :: RealFrac x => QuadTree k x -> V2 x -> (Int, Int)
locToZone qt (V2 x y) =
  let (sx, sy) = getStride qt
   in (floor $ x / sx, floor $ y / sy)


------------------------------------------------------------------------------
-- | Remove a key from the quadtree.
remove :: (RealFrac x, Ord k) => QuadTree k x -> k -> QuadTree k x
remove qt@QuadTree{..} k =
  case M.lookup k qtMap of
    Just p ->
      let pt = locToZone qt p
       in qt
          { qtMap   = M.delete k qtMap
          , qtSpace = qtSpace & ix pt %~ S.delete k
          }
    Nothing -> qt


------------------------------------------------------------------------------
-- | Reomve the size of a key from the quadtree.
removeSize :: (RealFrac x, Ord k) => QuadTree k x -> k -> QuadTree k x
removeSize qt@QuadTree{..} k = qt { qtEntSize = M.delete k qtEntSize }


------------------------------------------------------------------------------
-- | Insert a key with a location into the quadtree.
insert :: (RealFrac x, Ord k) => QuadTree k x -> k -> V2 x -> QuadTree k x
insert qt@QuadTree{..} k p =
  let pt = locToZone qt p
   in qt
      { qtMap   = M.insert k p qtMap
      , qtSpace = qtSpace & ix pt %~ S.insert k
      }


------------------------------------------------------------------------------
-- | Move a key to a new location in the quadtree.
move :: (RealFrac x, Ord k) => QuadTree k x -> k -> V2 x -> QuadTree k x
move qt k = insert (remove qt k) k


------------------------------------------------------------------------------
-- | Get the TL and BR points of a zone.
zoneBB :: RealFrac x => QuadTree k x -> (Int, Int) -> (V2 x, V2 x)
zoneBB qt (x, y) =
  let (sx, sy) = getStride qt
      x' = fromIntegral x
      y' = fromIntegral y
   in ( V2 (sx * x')
           (sy * y')
      , V2 (sx * (x' + 1))
           (sy * (y' + 1))
      )


------------------------------------------------------------------------------
-- | Get all zones
zones :: QuadTree k x -> [(Int, Int)]
zones = indices . qtSpace


------------------------------------------------------------------------------
-- | Get all zones we need to investigate
getZones :: RealFrac x => QuadTree k x -> (Int, Int) -> [(Int, Int)]
getZones qt@QuadTree{..} (x, y) = do
  let (xx, xy) = getExtraZones qt
  dx <- [-xx..xx]
  dy <- [-xy..xy]
  let z = (x+dx, y+dy)
  guard $ okZone qt z
  pure z


------------------------------------------------------------------------------
-- | Get the keys in a zone.
inZone :: (Ord k, RealFrac x) => QuadTree k x -> (Int, Int) -> [(k, V2 x, x)]
inZone qt@QuadTree{..} i = do
  z <- getZones qt i
  k <- S.toList $ qtSpace ! z
  loc <- maybe [] pure $ getLoc qt k
  let size = maybe 0 id $ getSize qt k
      tile = zoneBB qt z

  guard $ z == i || rectCircleIntersect loc size tile
  pure (k, loc, size)


------------------------------------------------------------------------------
-- | Is a given point in a rect?
pointInRect :: Ord x => V2 x -> (V2 x, V2 x) -> Bool
pointInRect (V2 x y) (V2 lx ly, V2 hx hy) =
  and [ lx <= x
      , x  <  hx
      , ly <= y
      , y  <  hy
      ]


------------------------------------------------------------------------------
-- | Is a given point in a circle?
pointInCircle :: (Ord x, Num x) => V2 x -> V2 x -> x -> Bool
pointInCircle p1 p2 r = quadrance (p1 - p2) < r * r


------------------------------------------------------------------------------
-- | Does a circle intersect a rect?
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


------------------------------------------------------------------------------
-- | Do two rects intersect?
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


------------------------------------------------------------------------------
-- | Get any keys within a distance of a given point.
inRange :: (Ord k, RealFrac x) => QuadTree k x -> V2 x -> x -> [(k, V2 x)]
inRange qt@QuadTree{..} p rd = do
  let allTiles  = fmap (\i -> (i, zoneBB qt i)) $ indices qtSpace
  (z, _) <- filter (rectCircleIntersect p rd . snd) allTiles
  (k, loc, size) <- inZone qt z
  guard . pointInCircle p loc $ size + rd
  pure (k, loc)


------------------------------------------------------------------------------
-- | Get all keys in a given rect.
inRect :: (Ord k, RealFrac x) => QuadTree k x -> (V2 x, V2 x) -> [(k, V2 x)]
inRect qt@QuadTree{..} rect = do
  let allTiles  = fmap (\i -> (i, zoneBB qt i)) $ indices qtSpace
  (z, tile) <- filter (rectRectIntersect rect . snd) allTiles
  (k, loc, size) <- inZone qt z
  guard . rectCircleIntersect loc size $ tile
  pure (k, loc)

