{-# LANGUAGE NoImplicitPrelude #-}

module GameData where

import AbilityUtils
import Overture


mePlayer :: Player
mePlayer = Player $ rgb 1 0 0


neutralPlayer :: Player
neutralPlayer = Player $ rgb 0.25 0.55 0.95


gunAttackData :: Attack
gunAttackData = Attack
  { _aCooldown  = Limit 0 0.75
  , _aRange     = 75
  , _aTask      = missile (missileEnt 300) $ \v2 t -> do
      doDamage Nothing 30 v2 t
      explosion v2 1 $ \d -> scale (d + 0.01)
                           . filled (rgba 1 0 0 $ 1 - d / 2)
                           . circle
                           $ 8 + d * 3
  }


attackAction :: Action
attackAction = Action
  { _acName   = "Attack"
  , _acHotkey = Just AKey
  , _acTType  = TargetTypeUnit ()
  , _acTask   = \e t -> lift $ setEntity e unchanged { target = Set t }
  }


stopAction :: Action
stopAction = Action
  { _acName   = "Stop"
  , _acHotkey = Just SKey
  , _acTType  = TargetTypeInstant ()
  , _acTask   = \e _ ->
      lift $ setEntity e unchanged
        { target  = Unset
        , pathing = Unset
        }
  }


psiStormAction :: Action
psiStormAction = Action
  { _acName   = "Psi Storm"
  , _acHotkey = Just TKey
  , _acTType  = TargetTypeGround ()
  , _acTask   = psiStorm
  }


psiStorm :: Ability
psiStorm _ (TargetUnit {}) = error "no can do"
psiStorm _ (TargetGround v2) = do
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

