{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Behavior where

-- import Data.Functor.Foldable (hylo)
import Overture
import qualified Data.Vector as V


data BinTree s a = Leaf s | Branch a a
  deriving Functor


biggestDude :: Num t => t
biggestDude = 10

sqr :: Num a => a -> a
sqr x = x * x


data MoveCmd = MoveCmd [V2]

instance IsLocationCommand MoveCmd where
  fromLocation e g = do
    [p] <- efor (anEnt e) $ query pos
    mpp <- findPath p g
    pure $ case mpp of
      Just pp -> Success $ MoveCmd pp
      Nothing -> Attempted

instance IsCommand MoveCmd where
  pumpCommand _ _ (MoveCmd []) = pure Nothing
  pumpCommand dt e (MoveCmd gg@(g:gs)) = do
    [gg'] <- eover (anEnt e) $ do
      (notThereYet, p) <- moveTowards dt g

      pure . (bool gs gg notThereYet, ) $ unchanged
        { pos = Set p
        }
    pure . Just $ MoveCmd gg'


-- data AttackCmd = AttackCmd
--   { _aTarget    :: Ent
--   , _aCooldown  :: Limit Time
--   , _aRange     :: Double
--   , _aTask      :: Ent -> Ent -> Task ()
--   }



-- followOrder :: Time -> Ent -> Action -> Game ()
-- followOrder dt e (MoveAction t) =
--   emap (anEnt e) $ moveOrder dt t

-- followOrder _ e StopAction = clearOrder e

-- followOrder dt e (AttackAction t) = do
--   runQueryT t (queryMaybe pos) >>= \case
--     Nothing -> clearOrder e
--     Just tpos -> do
--       ent <- getEntity e
--       let unQueryT' m = unQueryT m e ent
--       ups <- unQueryT' $ do
--         p <- query pos
--         a <- query attack

--         -- TODO(sandy): how to do attack ground?
--         let cooldown'  = a ^. aCooldown.limVal - dt
--             refresh    = cooldown' < 0
--             rng        = a ^. aRange
--             dst        = fmap (quadrance . (p -)) tpos
--             refresh'   = refresh && maybe False (<= rng * rng) dst
--             cooldown'' = bool cooldown' (a ^. aCooldown.limMax) refresh'
--             action     = bool Nothing (Just $ _aTask a e (TargetUnit t)) refresh'

--         pure $ (action,) $ unchanged
--           { attack = Set $ a & aCooldown.limVal .~ cooldown''
--           }

--       for_ ups $ \(task, setter) -> do
--         setEntity e setter
--         for_ task start




findPath :: MonadState LocalState m => V2 -> V2 -> m (Maybe [V2])
findPath src dst = do
  nm <- gets $ mapNavMesh . _lsMap
  let dst' = dst ^. from centerTileScreen
      thepath = nmFind nm (src ^. from centerTileScreen)
                          dst'
  pure $ if nmIsOpen nm dst'
     then thepath <&> fmap ((+ halfTile) . view centerTileScreen) . shorten nm
     else Nothing


sweep :: NavMesh -> (Int, Int) -> (Int, Int) -> Bool
sweep nm (sx, sy) (gx, gy) =
  let src = fmap fromIntegral $ V2 sx sy :: V2
      dst = fmap fromIntegral $ V2 gx gy
      diff = dst - src
      dist = abs (sx - gx) + (sy - gy)
      dir  = normalize diff
   in all (nmIsOpen nm)
        [ (floor x, floor y)
        | n <- [0 .. dist]
        , let v = src + dir ^* fromIntegral n
              x = view _x v
              y = view _y v
        ]


shorten :: NavMesh -> [(Int, Int)] -> [(Int, Int)]
shorten nm = id -- V.toList . hylo alg coalg . V.fromList
  where
    _alg (Leaf a)     = a
    _alg (Branch a b) = a <> b

    _coalg v = case V.length v of
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


setOrder :: Command -> EntWorld 'SetterOf
setOrder o = unchanged
  { command   = Set o
  }

resolveAttempt
    :: (Typeable a, IsCommand a)
    => Ent
    -> Attempt a
    -> Game ()
resolveAttempt e (Success cmd) = do
  emap (anEnt e) $ pure unchanged
    { command = Set $ SomeCommand cmd }
resolveAttempt _ Attempted = pure ()
resolveAttempt _ (Failure err) = do
  _ <- pure $ showTrace err
  pure ()


getSelectedEnts :: Game [Ent]
getSelectedEnts = efor aliveEnts $
  with selected *> queryEnt

