{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Behavior where

import Overture


data MoveCmd = MoveCmd [V2]

data StopCmd = StopCmd

data AttackCmd = AttackCmd
  { _acIx     :: Int
  , _acTarget :: Ent
  , _acPath   :: Maybe MoveCmd
  , _acRepath :: Time
  }

makeLenses ''AttackCmd



biggestDude :: Num t => t
biggestDude = 10

sqr :: Num a => a -> a
sqr x = x * x

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


instance IsInstantCommand StopCmd where
  fromInstant _ = pure $ pure StopCmd

instance IsCommand StopCmd where
  pumpCommand _ _ _ = pure Nothing


matches :: Eq a => Maybe a -> Maybe a -> Bool
matches a b = maybe True id $ (==) <$> a <*> b

acRepathTime :: Time
acRepathTime = 0.2


instance IsUnitCommand AttackCmd where
  fromUnit e t = do
    mas <- eon e $ query attacks
    case mas of
      Nothing -> pure $ Failure "No attackdata!"
      Just as -> do
        c <- eon t $ query classification
        case listToMaybe . filter (any (matches c) . _aClass . fst)
                         $ zip as [0..] of
          Just (_, ixx) ->
            pure . pure $ AttackCmd ixx t Nothing 0
          Nothing  -> pure . Failure $ "Unable to attack" ++ show c

instance IsCommand AttackCmd where
  pumpCommand dt e ac@AttackCmd{..} = do
    let t = _acTarget
    eon t (with isAlive >> query pos) >>= \case
      Nothing -> pure Nothing
      Just tp -> do
        -- get attackdata
        Just (p, a) <- eon e $
          (,) <$> query pos <*> query (fmap (!! _acIx) . attacks)

        let cooldown' = a ^. aCooldown.limVal - dt
            rng       = a ^. aRange
            dst       = quadrance $ p - tp

        case dst <= rng * rng of
          -- if in range
          True -> do
            let refresh    = cooldown' < 0
                cooldown'' = bool cooldown' (a ^. aCooldown.limMax) refresh
                action     = bool Nothing (Just $ _aTask a e (TargetUnit t)) refresh
            for_ action start
            emap (anEnt e) $ do
              as <- query attacks
              pure unchanged
                { attacks = Set $ as & ix _acIx . aCooldown.limVal .~ cooldown''
                }
            pure . Just $
              ac & acPath .~ Nothing

          -- not in range
          False -> do
            -- needs to repath
            case (isNothing _acPath || _acRepath <= 0) of
              True -> do
                fromLocation @MoveCmd e tp >>= \case
                  Success mcmd ->
                    pure . Just $ ac & acPath ?~ mcmd
                                     & acRepath .~ acRepathTime
                  _ -> pure Nothing

              -- has a path
              False -> do
                Just mcmd <- pure _acPath
                mcmd' <- pumpCommand dt e mcmd
                pure . Just $ ac & acPath .~ mcmd'
                                 & acRepath -~ dt


findPath :: MonadState LocalState m => V2 -> V2 -> m (Maybe [V2])
findPath src dst = do
  nm <- gets $ mapNavMesh . _lsMap
  let dst' = dst ^. from centerTileScreen
      thepath = nmFind nm (src ^. from centerTileScreen)
                          dst'
  pure $ if nmIsOpen nm dst'
     then thepath <&> fmap ((+ halfTile) . view centerTileScreen)
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

