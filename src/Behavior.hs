{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Behavior where

import AbilityUtils
import Control.Monad.Trans.Maybe
import Overture

data PsiStormCmd = PsiStormCmd V2
  deriving Typeable

data PassiveScriptCmd = PassiveScriptCmd Int
  deriving Typeable

data UnitScriptCmd = UnitScriptCmd Int
  deriving Typeable

data MoveCmd = MoveCmd
  { _mcWaypoints :: [V2]
  , _mcGoal      :: V2
  , _mcStuckTime :: Time
  }
  deriving (Typeable)

data StopCmd = StopCmd
  deriving (Typeable)

data TrainCmd = TrainCmd Proto
  deriving Typeable

data AttackCmd = AttackCmd
  { _acIx     :: Int
  , _acTarget :: Ent
  , _acPath   :: Maybe (V2, MoveCmd)
  , _acRepath :: Time
  } deriving (Typeable)

data AcquireCmd = AcquireCmd
  { _aqAttack :: AttackCmd
  , _aqPos    :: V2
  , _aqDist   :: Double
  } deriving (Typeable)

data BuildCmd = BuildCmd
  { _bcProto :: Proto
  , _bcPos   :: V2
  , _bcMove  :: Maybe MoveCmd
  } deriving (Typeable)


makeLenses ''AttackCmd
makeLenses ''AcquireCmd
makeLenses ''BuildCmd
makeLenses ''MoveCmd



instance IsCommand TrainCmd where
  type CommandParam TrainCmd = Proto
  pumpCommand _ e (TrainCmd proto) = do
    Just (p, (_, ysize), o) <-
      eon e $ (,,) <$> query pos
                   <*> query gridSize
                   <*> query owner

    let desired = p + V2 0 (fromIntegral (ysize + 1) * tileHeight)
        s       = maybe defSize id $ entSize proto
    actual <- closestPointTo desired s $ V2 1 0

    void $ createEntity proto
     { pos   = Just actual
     , owner = Just o
     }
    pure Nothing

instance IsInstantCommand TrainCmd where
  fromInstant = pure . pure . pure . TrainCmd


instance IsCommand UnitScriptCmd where
  type CommandParam UnitScriptCmd = Ent -> Ent -> Task ()
  pumpCommand _ _ a = pure $ Just a
  endCommand _ (Just (UnitScriptCmd a)) = stop a
  endCommand _ Nothing = pure ()

instance IsUnitCommand UnitScriptCmd where
  fromUnit t e u =
    fmap (Success . UnitScriptCmd) . start $ t e u


instance IsCommand PassiveScriptCmd where
  type CommandParam PassiveScriptCmd = Ent -> Task ()
  pumpCommand _ _ a = pure $ Just a
  endCommand _ (Just (PassiveScriptCmd i)) = stop i
  endCommand _ Nothing = pure ()

instance IsInstantCommand PassiveScriptCmd where
  fromInstant t e =
    fmap (Success . PassiveScriptCmd) . start $ t e


instance IsPlacementCommand BuildCmd where
  fromPlacement proto e i = do
    let dst = i ^. centerTileScreen
    fromLocation @MoveCmd () e dst >>= pure . \case
      Success cmd -> Success $ BuildCmd proto dst $ Just cmd
      _           -> Failure "Unable to get to building location"

instance IsCommand BuildCmd where
  type CommandParam BuildCmd = Proto
  pumpCommand dt e bc@BuildCmd{..} = case _bcMove of
    Just cmd -> do
      cmd' <- pumpCommandImpl dt e cmd
      pure $ Just $ bc & bcMove .~ cmd'

    Nothing -> do
      Just o <- eon e $ query owner
      void . createEntity $ _bcProto
        { pos   = Just _bcPos
        , owner = Just o
        }
      setEntity e unchanged
        { pos = Set $ _bcPos - V2 0 tileHeight
        }
      pure Nothing

instance IsLocationCommand PsiStormCmd where
  fromLocation _ _ = pure . pure . PsiStormCmd

instance IsCommand PsiStormCmd where
  pumpCommand _ _ (PsiStormCmd v2) = do
    void . start $ do
      let size       = 100
          dmg        = 100
          flashTime  = 0.1
          waitPeriod = 0.75
          cycles     = 4

      let add  = V2 size size ^* 0.5
          p1   = v2 - add
          p2   = v2 + add
          form = rect size size

      lift . explosion v2 (waitPeriod * cycles)
          . const
          $ filled (rgba 0 0.8 1 0.3) form
      for_ [0 .. cycles - 1] . const $ do
        wait waitPeriod
        lift $ do
          explosion v2 flashTime
            . const
            $ filled (rgb 0 0.8 1) form
          inRange <- getUnitsInSquare p1 p2
          eover (someEnts inRange)
            . fmap ((),)
            $ performDamage dmg
    pure Nothing

instance IsInstantCommand AcquireCmd where
  fromInstant _ e = do
    Just (p, acq, o) <-
      eon e $ (,,) <$> query pos
                   <*> query acqRange
                   <*> query owner
    badGuys <- efor (fmap fst <$> getUnitsInRange p acq) $ do
        o' <- query owner
        guard $ isEnemy o o'
        (,) <$> queryEnt
            <*> query pos
    let bads = sortBy (comparing $ quadrance . (p - ) . snd) badGuys
    -- TODO(sandy): check to make sure you can attack this guy!
    case listToMaybe bads of
      Just (t, _) -> do
        fromUnit @AttackCmd () e t >>= \case
          Success atk -> pure $ pure $ AcquireCmd atk p acq
          _ -> pure Attempted

      Nothing -> pure Attempted

instance IsCommand AcquireCmd where
  pumpCommand dt e aq@AcquireCmd{..} = do
    Just p <- eon e $ query pos
    case fastInRange (p - _aqPos) _aqDist of
      True ->
        pumpCommandImpl dt e _aqAttack >>= pure . \case
          Just acmd' -> Just $ aq & aqAttack .~ acmd'
          Nothing    -> Nothing
      False -> pure Nothing



isEnemy :: Player -> Player -> Bool
isEnemy = (/=)


sqr :: Num a => a -> a
sqr x = x * x


instance IsLocationCommand MoveCmd where
  fromLocation _ e g =
    (>>= maybe (pure Attempted) (pure . Success)) . runMaybeT $ do
      (p, flying) <- MaybeT . eon e $ (,) <$> query pos
                                          <*> queryFlag isFlying
      pp <- bool (MaybeT . lift $ findPath p g) (pure [g]) flying
      lift $ playAnim e [AnimWalk, AnimIdle]
      pure $ MoveCmd pp g 0

maxStuckTime :: Time
maxStuckTime = 0.2


tryMoveInto :: Ent -> V2 -> Double -> V2 -> Bool -> Game (Maybe (Ent, V2))
tryMoveInto e p sz dir flying = do
  obstructions <-
    bool (getPointObstructions p sz $ Just e) (pure []) flying

  when (null obstructions) $ do
    setEntity e unchanged
      { pos     = Set p
      , lastDir = Set dir
      }

  pure $ listToMaybe obstructions


instance IsCommand MoveCmd where
  pumpCommand _ _ (MoveCmd [] _ _) = pure Nothing
  pumpCommand dt e mcmd@MoveCmd{..} = do
    let gg@(g:gs) = _mcWaypoints
    ent <- getEntity e
    let Just p = pos ent
        Just s = speed ent
        sz     = maybe defSize id $ entSize ent
        flying = fromFlag $ isFlying ent

        travel = s * dt
        sub = g - p
        dist = norm sub
        isThere = dist <= travel
        dir = normalize sub
        p' = p + dir ^* min travel dist

    case _mcStuckTime < maxStuckTime of
      True -> do
        success <- isNothing <$> tryMoveInto e p' sz dir flying

        pure . pure $ case success of
          True  -> mcmd & mcWaypoints .~ bool gg gs isThere
          False -> mcmd & mcStuckTime +~ dt

      False -> do
        obstruction <- tryMoveInto e p' sz dir flying
        case obstruction of
          Nothing ->
            pure . pure $ mcmd & mcStuckTime .~ 0
          Just (_, op) -> do
            let d = op - p
                dir' = normalize $ rotV2 (pi / 2) d
                p'' = p + dir' ^* travel

            success <- isNothing <$> tryMoveInto e p'' sz dir' flying

            pure $ case success of
              False -> Nothing  -- fuck it
              True  ->
                pure $ mcmd & mcWaypoints %~ (p + dir' ^* sz * 3:)
                            & mcStuckTime .~ 0



  endCommand e _ = playAnim e [AnimIdle]


instance IsInstantCommand StopCmd where
  fromInstant _ _ = pure $ pure StopCmd

instance IsCommand StopCmd where
  pumpCommand _ _ _ = pure Nothing


matches :: Eq a => Maybe a -> Maybe a -> Bool
matches a b = maybe True id $ (==) <$> a <*> b

acRepathTime :: Time
acRepathTime = 0.2


instance IsUnitCommand AttackCmd where
  fromUnit _ e t = do
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
  endCommand e _ = playAnim e [AnimIdle]
  pumpCommand dt e ac@AttackCmd{..} = runMaybeT $ do
    let t = _acTarget

    tp <- MaybeT . eon t $ with isAlive >> query pos
    (p, a) <- MaybeT . eon e $ (,) <$> query pos
                                   <*> query (fmap (!! _acIx) . attacks)

    -- TODO(sandy): cooldown only updates when you have an attack cmd
    let cooldown' = a ^. aCooldown.limVal - dt
        rng       = a ^. aRange
        direction = tp - p

    case fastInRange direction rng of
      -- if in range
      True -> lift $ do
        let refresh    = cooldown' < 0
            cooldown'' = bool cooldown' (a ^. aCooldown.limMax) refresh
            action     = bool Nothing (Just $ _aTask a e (TargetUnit t)) refresh
        for_ action $ \task -> do
          playAnim e [AnimAttack, AnimIdle]
          start task

        setEntity e unchanged
          { attacks = Modify $ ix _acIx . aCooldown.limVal .~ cooldown''
          , lastDir = Set $ normalize direction
          }
        pure $ ac & acPath .~ Nothing

      -- not in range
      False -> do
        -- needs to repath
        case (isNothing _acPath
            || (_acRepath <= 0
             && not (fastInRange (fst (fromJust _acPath) - tp) rng))) of
          True -> do
            mcmd <- MaybeT . fmap attemptToMaybe
                           $ fromLocation @MoveCmd () e tp
            pure $ ac & acPath   ?~ (tp, mcmd)
                      & acRepath .~ acRepathTime

          -- has a path
          False -> do
            (g, mcmd) <- hoistMaybe _acPath
            mcmd' <- lift $ pumpCommandImpl dt e mcmd
            pure $ ac & acPath .~ fmap (g,) mcmd'
                      & acRepath -~ dt

fastInRange :: V2 -> Double -> Bool
fastInRange dst rng = quadrance dst <= rng * rng


findPath :: MonadState LocalState m => V2 -> V2 -> m (Maybe [V2])
findPath src dst = do
  nm <- gets _lsNavMesh
  let dst' = dst ^. from centerTileScreen
      thepath = nmFind nm (src ^. from centerTileScreen)
                          dst'
  pure $ if nmIsOpen nm dst'
     then thepath <&> fmap ((+ halfTile) . view centerTileScreen)
     else Nothing


moveTowards :: Time -> V2 -> Query (Bool, V2, V2)
moveTowards dt g = do
  p <- query pos
  s <- query speed

  let dir = g - p
      dist = norm dir
      dx = s * dt
      dir' = normalize dir

  pure (dx < dist, p + dx *^ dir', dir')


getSelectedEnts :: Game [Ent]
getSelectedEnts = efor aliveEnts $
  with selected *> queryEnt

directMoveCommand :: V2 -> Command
directMoveCommand p = SomeCommand $ MoveCmd [p] p 0
