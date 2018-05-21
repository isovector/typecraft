{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Behavior where

import Data.Functor.Foldable (hylo)
import Overture
import qualified Data.Vector as V


data BinTree s a = Leaf s | Branch a a
  deriving Functor


biggestDude :: Num t => t
biggestDude = 10

sqr :: Num a => a -> a
sqr x = x * x


findPath :: MonadState LocalState m => V2 -> V2 -> m (Maybe [V2])
findPath src dst = do
  nm <- gets $ mapNavMesh . _lsMap
  let thepath = nmFind nm (src ^. from centerTileScreen)
                          (dst ^. from centerTileScreen)
  pure $ thepath <&> fmap (view centerTileScreen) . shorten nm


sweep :: NavMesh -> (Int, Int) -> (Int, Int) -> Bool
sweep nm (sx, sy) (gx, gy) =
  let src = fmap fromIntegral $ V2 sx sy :: V2
      dst = fmap fromIntegral $ V2 gx gy
      diff = dst - src
      dist = abs (sx - gx) + (sy - gy)
      dir  = normalize diff
   in all (not . nmTest nm)
        [ (floor x, floor y)
        | n <- [0 .. dist]
        , let v = src + dir ^* fromIntegral n
              x = view _x v
              y = view _y v
        ]


shorten :: NavMesh -> [(Int, Int)] -> [(Int, Int)]
shorten nm = id -- V.toList . hylo alg coalg . V.fromList
  where
    alg (Leaf a)     = a
    alg (Branch a b) = a <> b

    coalg v = case V.length v of
      0 -> Leaf V.empty
      1 -> Leaf v
      x ->
        let va = V.head v
            vz = V.last v
            half = x `div` 2
         in if sweep nm va vz
               then Leaf $ V.singleton vz
               else Branch (V.slice 0 half v)
                           (V.slice half (x - half) v)




-- doMoveCollide :: Time -> Ent -> Game ()
-- doMoveCollide dt ent = do
--   z <- runQueryT ent $ do
--     Path (p : ps) <- query pathing
--     (done, pos') <- moveTowards dt p
--     size         <- query entSize

--     pure (done, pos', size)

--   for_ z $ \(done, pos', size) -> do
--     us <- fmap (fmap fst) . getUnitsInRange pos' $ size + biggestDude
--     udata <- efor (someEnts us) $
--       (,,) <$> queryEnt
--            <*> query pos
--            <*> query entSize
--     let actualCollisions = flip filter udata $ \(e, p, s) ->
--           quadrance (pos' - p) <= sqr (size + s)
--     undefined





moveTowards :: Time -> V2 -> Query (Bool, V2)
moveTowards dt g = do
  p <- query pos
  s <- query speed

  let dir = g - p
      dist = norm dir
      dx = s * dt

  pure (dx < dist, p + dx *^ normalize dir)

