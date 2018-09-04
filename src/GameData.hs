{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module GameData where

import AbilityUtils
import AnimBank
import Behavior
import Behavior.Scripts
import Overture


marineProto :: Proto
marineProto = newEntity
  { attacks  = Just [gunAttackData]
  , entSize  = Just 7
  , acqRange = Just 125
  , speed    = Just 150
  , unitType = Just Unit
  , hp       = Just $ Limit 100 100
  , commands = Just $ harvestWidget : stdWidgets
  }

garethProto :: Proto
garethProto = newEntity
  { attacks  = Just [changAttackData]
  , entSize  = Just 7
  , acqRange = Just 200
  , speed    = Just 100
  , unitType = Just Unit
  , hp       = Just $ Limit 100 100
  , commands = Just $ stdWidgets
  , animBundle = Just
      [ (AnimAttack, __garethAttack)
      , (AnimIdle, __garethIdle)
      ]
  }

mineralsProto :: Proto
mineralsProto = newEntity
  { gfx = Just . move (V2 (tileWidth * 1.5) (tileHeight))
               . filled (rgb 0 0 0)
               $ rect (tileWidth * 3) (tileHeight * 2)
  , entSize  = Just 14
  , unitType = Just Building
  , gridSize = Just (3, 2)
  , resourceSource = Just (Minerals, pure 1000)
  }



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
  , commands       = Just [trainMarineWidget]
  , isDepot        = Just ()
  }


gunAttackData :: AttackData
gunAttackData = AttackData
  { _aCooldown = Limit 0 0.75
  , _aRange    = 75
  , _aClass    = [Nothing]
  , _aTask     = missile (missileProto 300) $ \v2 t -> do
      doDamage Nothing 30 v2 t
      explosion v2 1 $ \d -> scale (d + 0.01)
                           . filled (rgba 1 0 0 $ 1 - d / 2)
                           . circle
                           $ 8 + d * 3
  }

changAttackData :: AttackData
changAttackData = AttackData
  { _aCooldown = Limit 0 2
  , _aRange    = 150
  , _aClass    = [Nothing]
  , _aTask     = missile changEnt $ \v2 t -> do
      doDamage Nothing 100 v2 t
      explosion v2 1 $ \d -> scale (d + 0.01)
                           . filled (rgba 1 0 0 $ 1 - d / 2)
                           . circle
                           $ 8 + d * 3
  }
  where
    changEnt =
      (missileProto 300)
        { gfx = Just . scale 0.1 . toForm $ image "assets/chang.png"
        }

psiStormWidget :: CommandWidget
psiStormWidget = CommandWidget
  { cwName    = "Psi Storm"
  , cwCommand = LocationCommand $ Proxy2 @PsiStormCmd ()
  , cwPos     = Just (Col1, Row3)
  , cwHotkey  = Just TKey
  }


moveWidget :: CommandWidget
moveWidget = CommandWidget
  { cwName    = "Move"
  , cwCommand = LocationCommand $ Proxy2 @MoveCmd ()
  , cwPos     = Just (Col1, Row1)
  , cwHotkey  = Just MKey
  }

attackWidget :: CommandWidget
attackWidget = CommandWidget
  { cwName    = "Attack"
  , cwCommand = UnitCommand $ Proxy2 @AttackCmd ()
  , cwPos     = Just (Col4, Row1)
  , cwHotkey  = Just AKey
  }

stopWidget :: CommandWidget
stopWidget = CommandWidget
  { cwName    = "Stop"
  , cwCommand = InstantCommand $ Proxy2 @StopCmd ()
  , cwPos     = Just (Col2, Row1)
  , cwHotkey  = Just SKey
  }

acquireWidget :: CommandWidget
acquireWidget = CommandWidget
  { cwName    = "Acquire"
  , cwCommand = InstantCommand $ Proxy2 @AcquireCmd ()
  , cwPos     = Nothing
  , cwHotkey  = Nothing
  }

buildCommandCenterWidget :: CommandWidget
buildCommandCenterWidget = CommandWidget
  { cwName    = "Build Command Center"
  , cwCommand = PlacementCommand $ Proxy2 @BuildCmd commandCenter
  , cwPos     = Just (Col1, Row2)
  , cwHotkey  = Just CKey
  }

volcanoPassiveWidget :: CommandWidget
volcanoPassiveWidget = CommandWidget
  { cwName    = "Eruption"
  , cwCommand = PassiveCommand $ Proxy2 @PassiveScriptCmd volcanoPassive
  , cwPos     = Nothing
  , cwHotkey  = Nothing
  }

trainMarineWidget :: CommandWidget
trainMarineWidget = CommandWidget
  { cwName    = "Train Marine"
  , cwCommand = InstantCommand $ Proxy2 @TrainCmd marineProto
  , cwPos     = Just (Col1, Row1)
  , cwHotkey  = Just AKey
  }

harvestWidget :: CommandWidget
harvestWidget = CommandWidget
  { cwName    = "Harvest"
  , cwCommand = UnitCommand $ Proxy2 @UnitScriptCmd harvestScript
  , cwPos     = Just (Col4, Row2)
  , cwHotkey  = Just HKey
  }


stdWidgets :: [CommandWidget]
stdWidgets = [moveWidget, stopWidget, attackWidget, acquireWidget]

