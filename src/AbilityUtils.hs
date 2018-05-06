{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AbilityUtils where

import Overture hiding (init)


findTarget :: Target -> Game (Maybe V2)
findTarget (TargetGround v2) = pure $ Just v2
findTarget (TargetUnit e)    = fmap pos $ getEntity e


doDamage :: Double -> Int -> DamageHandler
doDamage splash dmg v2 (TargetUnit e) = do
  if splash == 0
     then void . eover (anEnt e)
               . const
               . fmap ((),)
               $ performDamage dmg
     else doDamage splash dmg v2 $ TargetGround v2
doDamage splash dmg v2 (TargetGround {}) = do
  units <- fmap fst <$> getUnitsInRange v2 splash
  void . eover (someEnts units)
       . const
       . fmap ((),)
       $ performDamage dmg


performDamage :: Int -> Query (EntWorld 'SetterOf)
performDamage dmg = do
  health <- recv hp
  pure defEntity'
    { hp = Set $ health & limVal -~ dmg
    }


missile :: EntWorld 'FieldOf -> DamageHandler -> Ability
missile proto fx attacker t = do
  mpos0 <- lift $ fmap pos $ getEntity attacker
  mtpos <- lift $ findTarget t
  case (mpos0, mtpos) of
    (Just pos0, Just tpos) -> do
      ment <- lift $ newEntity proto
        { pos     = Just pos0
        , pathing = Just $ Goal tpos
        }
      waitUntil $ do
        me <- getEntity ment
        pure . isNothing $ pathing me
      lift $ do
        Just pos' <- fmap pos $ getEntity ment
        fx pos' t
        setEntity ment delEntity
    _ -> pure ()


missileEnt :: Double -> Proto
missileEnt sp = defEntity
  { unitType = Just Missile
  , speed    = Just sp
  }

