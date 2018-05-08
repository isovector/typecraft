{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AbilityUtils where

import Overture hiding (init)


explosion :: V2 -> Time -> (Double -> Form) -> Game ()
explosion p dur draw = do
  me <- newEntity $ defEntity
    { gfx = Just $ draw 0
    }
  setPos me p
  start $ do
    during dur $ \delta ->
      lift $ setEntity me defEntity'
        { gfx = Set $ draw delta
        }
    lift $ setEntity me delEntity


findTarget :: Target -> Game (Maybe V2)
findTarget (TargetGround v2) = pure $ Just v2
findTarget (TargetUnit e)    = getPos e


doDamage :: Maybe Double -> Int -> DamageHandler
doDamage splash dmg v2 (TargetUnit e) = do
  if splash == Nothing
     then void . eover (anEnt e)
               . const
               . fmap ((),)
               $ performDamage dmg
     else doDamage splash dmg v2 $ TargetGround v2
doDamage (Just splash) dmg v2 (TargetGround {}) = do
  units <- fmap fst <$> getUnitsInRange v2 splash
  void . eover (someEnts units)
       . const
       . fmap ((),)
       $ performDamage dmg
doDamage Nothing _ _ (TargetGround {}) = pure ()


performDamage :: Int -> Query (EntWorld 'SetterOf)
performDamage dmg = do
  health <- recv hp
  pure defEntity'
    { hp = Set $ health & limVal -~ dmg
    }


missile :: EntWorld 'FieldOf -> DamageHandler -> Ability
missile proto fx attacker t = do
  mpos0 <- lift $ getPos attacker
  mtpos <- lift $ findTarget t
  case (mpos0, mtpos) of
    (Just pos0, Just tpos) -> do
      ment <- lift $ newEntity proto
        { pathing = Just $ Goal tpos
        }
      lift $ setPos ment pos0
      waitUntil $ do
        me <- getEntity ment
        pure . isNothing $ pathing me
      lift $ do
        Just pos' <- getPos ment
        fx pos' t
        setEntity ment delEntity
    _ -> pure ()


missileEnt :: Double -> Proto
missileEnt sp = defEntity
  { unitType = Just Missile
  , speed    = Just sp
  }

