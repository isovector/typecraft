{-# LANGUAGE NoImplicitPrelude #-}

module GameData where

import           AbilityUtils
import           Behavior
import           Linear.Matrix
import qualified Linear.V2 as L
import           Overture


volcanoPassive :: Ent -> Task ()
volcanoPassive e = do
  Just v1 <- lift . eon e $ query pos
  let v2 = v1 + V2 40 0

  let sc         = 0.4
      size       = 20
      warning    = 0.2
      dmg        = 100
      waitPeriod = 0.75
      height     = V2 0 300

      volpos = v2 + V2 100 0 ^* sc

      rotmat theta = L.V2 (L.V2 (cos theta)          (sin theta))
                          (L.V2 (negate $ sin theta) (cos theta))

  flip fix [1..] $ \f z ->  do
    lift . explosion volpos waitPeriod
         $ \d -> scale (d + 0.01)
               . filled (rgba 1 0 0 $ 1 - d / 2)
               . circle
               $ 8 + d * 3

    let dx = rotmat (fromIntegral (head z) * pi / 302 * 45) !* (V2 200 0)
        pos = v2 + dx

    lift . explosion volpos warning
         $ \d -> move (-height ^* d)
               . scale 0.4
               . move (V2 (-32) (-30))
               . toForm
               $ image "assets/socks.png"

    wait 1

    lift . explosion pos 2
         $ \d -> scale (d + 0.01)
               . filled (rgba 0 0 0 $ 0.5 + d / 2)
               . circle
               $ 8 + d * 3

    wait $ 2 - warning

    lift . explosion pos warning
         $ \d -> move (-height + height ^* d)
               . scale 0.4
               . move (V2 (-32) (-30))
               . toForm
               $ image "assets/socks.png"

    wait warning

    void . lift $ do
      explosion pos waitPeriod
        $ \d -> scale (d + 0.01)
              . filled (rgba 1 0 0 $ 1 - d / 2)
              . circle
              $ 8 + d * 3

      inRange <- fmap fst <$> getUnitsInRange pos size
      eover (someEnts inRange)
          . fmap ((),)
          $ performDamage dmg

    wait waitPeriod

    f $ drop (head z `mod` 50) z


mePlayer :: Player
mePlayer = Player $ rgb 1 0 0


neutralPlayer :: Player
neutralPlayer = Player $ rgb 0.25 0.55 0.95


commandCenter :: Proto
commandCenter = newEntity
  { gfx            = Just $ toForm $ image "assets/cc.png"
  , gridSize       = Just (4, 3)
  , classification = Just BuildingUnit
  , unitType       = Just Building
  , hp             = Just $ Limit 100 100
  }


gunAttackData :: AttackData
gunAttackData = AttackData
  { _aCooldown = Limit 0 0.75
  , _aRange    = 75
  , _aClass    = [Nothing]
  , _aTask     = missile (missileEnt 300) $ \v2 t -> do
      doDamage Nothing 30 v2 t
      explosion v2 1 $ \d -> scale (d + 0.01)
                           . filled (rgba 1 0 0 $ 1 - d / 2)
                           . circle
                           $ 8 + d * 3
  }

psiStormWidget :: CommandWidget
psiStormWidget = CommandWidget
  { cwName = "Psi Storm"
  , cwCommand = LocationCommand $ Proxy2 @PsiStormCmd ()
  , cwVisible = True
  , cwHotkey = Just TKey
  }


moveWidget :: CommandWidget
moveWidget = CommandWidget
  { cwName = "Move"
  , cwCommand = LocationCommand $ Proxy2 @MoveCmd ()
  , cwVisible = True
  , cwHotkey = Just MKey
  }

attackWidget :: CommandWidget
attackWidget = CommandWidget
  { cwName = "Attack"
  , cwCommand = UnitCommand $ Proxy2 @AttackCmd ()
  , cwVisible = True
  , cwHotkey = Just AKey
  }

stopWidget :: CommandWidget
stopWidget = CommandWidget
  { cwName = "Stop"
  , cwCommand = InstantCommand $ Proxy2 @StopCmd ()
  , cwVisible = True
  , cwHotkey = Just SKey
  }

acquireWidget :: CommandWidget
acquireWidget = CommandWidget
  { cwName = "Acquire"
  , cwCommand = InstantCommand $ Proxy2 @AcquireCmd ()
  , cwVisible = False
  , cwHotkey = Nothing
  }

buildCommandCenterWidget :: CommandWidget
buildCommandCenterWidget = CommandWidget
  { cwName = "Build Command Center"
  , cwCommand = PlacementCommand $ Proxy2 @BuildCmd commandCenter
  , cwVisible = True
  , cwHotkey = Just CKey
  }

volcanoPassiveWidget :: CommandWidget
volcanoPassiveWidget = CommandWidget
  { cwName = "Eruption"
  , cwCommand = PassiveCommand . Proxy2 @PassiveScriptCmd $ volcanoPassive
  , cwVisible = False
  , cwHotkey = Nothing
  }

stdWidgets :: [CommandWidget]
stdWidgets = [moveWidget, stopWidget, attackWidget, acquireWidget]

