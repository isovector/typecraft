{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AbilityUtils where

import Overture hiding (init)


findTarget :: Target -> Game (Maybe V2)
findTarget (TargetGround v2) = pure $ Just v2
findTarget (TargetUnit e)    = fmap pos $ getEntity e


doDamage :: Int -> DamageHandler
doDamage dmg _ (TargetUnit e) =
  void . eover (anEnt e) . const $ do
    health <- recv hp
    pure . ((),) $ defEntity'
      { hp = Set $ health & limVal -~ dmg
      }
doDamage _ _ (TargetGround _) =
  error "can't damage ground"


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

