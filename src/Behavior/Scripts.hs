{-# LANGUAGE NoImplicitPrelude #-}

module Behavior.Scripts where

import AbilityUtils
import Behavior
import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Maybe
import Overture


harvestScript
    :: Ent
    -> Ent
    -> Task ()
harvestScript e h = fix $ \loop -> do
  let numPeriods = 5 :: Int
      periodTime = 0.3
      harvestAmount = 8

  let lifting = lift . lift
  (>>= maybe (pure ()) (const loop)) . runMaybeT $ do
    eEntity <- lifting $ getEntity e
    case powerup eEntity of
      Just ((rs, num)) -> do
        o <- hoistMaybe $ owner eEntity
        p <- hoistMaybe $ pos eEntity

        depots <- lifting . efor aliveEnts $ do
          with isDepot
          query owner >>= guard . (== o)
          (,) <$> queryEnt <*> query pos

        nearestDepot <- hoistMaybe
                      . fmap fst
                      . listToMaybe
                      . sortBy (comparing $ quadrance . (p -) . snd)
                      $ depots

        Just depotPos <- lifting . eon nearestDepot $ query pos
        Success mcmd <-
          lifting . fromLocation @MoveCmd () e
                  $ depotPos - V2 0 tileHeight
        lift $ runCommand e mcmd
        lifting $ do
          acquireResources o rs num
          setEntity e unchanged
            { powerup = Unset
            }

      Nothing -> do
        hEntity    <- lifting $ getEntity h
        harvestPos <- hoistMaybe $ pos hEntity
        rs         <- hoistMaybe $ resourceSource hEntity

        Success mcmd <-
          lifting . fromLocation @MoveCmd () e
                  $ harvestPos + V2 0 tileHeight ^* 2
        lift $ do
          runCommand e mcmd
          for_ [0..numPeriods] . const $ wait periodTime

        lifting $ do
          setEntity h unchanged
            { resourceSource = Modify $ _2 . limVal -~ harvestAmount
            }
          setEntity e unchanged
            { powerup = Set (fst rs, harvestAmount)
            }


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

  flip fix [1..] $ \f z ->  do
    lift . explosion volpos waitPeriod
         $ \d -> scale (d + 0.01)
               . filled (rgba 1 0 0 $ 1 - d / 2)
               . circle
               $ 8 + d * 3

    let dx = rotV2 (fromIntegral (head z) * pi / 302 * 45) $ V2 200 0
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

