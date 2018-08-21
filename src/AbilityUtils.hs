{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AbilityUtils where

import Overture hiding (init)
import {-# SOURCE #-} Behavior


explosion :: V2 -> Time -> (Double -> Form) -> Game ()
explosion p dur draw = do
  me <- createEntity $ newEntity
    { pos = Just p
    , gfx = Just $ draw 0
    }
  void . start $ do
    during dur $ \delta ->
      lift $ setEntity me unchanged
        { gfx = Set $ draw delta
        }
    lift $ setEntity me delEntity


findTarget :: Target -> Game (Maybe V2)
findTarget (TargetGround v2) = pure $ Just v2
findTarget (TargetUnit e)    = lift $ vgetPos e


doDamage :: Maybe Double -> Int -> DamageHandler
doDamage splash dmg v2 (TargetUnit e) = do
  if splash == Nothing
     then void . eover (anEnt e)
               . fmap ((),)
               $ performDamage dmg
     else doDamage splash dmg v2 $ TargetGround v2
doDamage (Just splash) dmg v2 (TargetGround {}) = do
  units <- fmap fst <$> getUnitsInRange v2 splash
  void . eover (someEnts units)
       . fmap ((),)
       $ performDamage dmg
doDamage Nothing _ _ (TargetGround {}) = pure ()


performDamage :: Int -> Query (EntWorld 'SetterOf)
performDamage dmg = do
  health <- query hp
  pure unchanged
    { hp = Set $ health & limVal -~ dmg
    }


missile :: EntWorld 'FieldOf -> DamageHandler -> Ability
missile proto fx attacker t = do
  mpos0 <- lift . lift $ vgetPos attacker
  mtpos <- lift $ findTarget t
  case (mpos0, mtpos) of
    (Just pos0, Just tpos) -> do
      ment <- lift $ createEntity proto
        { pos = Just pos0
        , currentCommand = Just $ directMoveCommand tpos
        }

      waitUntil $ do
        me <- getEntity ment
        pure . isNothing $ currentCommand me

      lift $ do
        Just pos' <- lift $ vgetPos ment
        fx pos' t
        setEntity ment delEntity
    _ -> pure ()


missileEnt :: Double -> Proto
missileEnt sp = newEntity
  { unitType = Just Missile
  , speed    = Just sp
  }

