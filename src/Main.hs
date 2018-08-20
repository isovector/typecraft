{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           AbilityUtils
import           Behavior
import           Client
import           Control.Monad.Trans.Writer (WriterT (..))
import           Control.Monad.Writer.Class (tell)
import qualified Data.DList as DL
import           Data.Ecstasy.Types (Ent (..), Hooks (..))
import qualified Data.Map as M
import           GameData
import           Linear.Matrix
import qualified Linear.V2 as L
import           Map
import           Overture hiding (init)
import           QuadTree.QuadTree (mkQuadTree)
import qualified QuadTree.QuadTree as QT


screenRect :: (V2, V2)
screenRect =
    ( V2 (-buffer)
         (-buffer)
    , V2 (gameWidth + buffer)
         (gameHeight + buffer)
    )
  where
    buffer = 64


separateTask :: Task ()
separateTask = do
  dyn0 <- lift $ gets _lsDynamic
  let zones = QT.zones dyn0
      howMany = 50 :: Int

  forever $ for_ (zip zones $ join $ repeat [0..howMany]) $ \(zone, i) -> do
    when (i == 0) $ void await
    ents <- lift $ getUnitsInZone zone
    let pairwise = do
          e1 <- ents
          e2 <- ents
          guard $ fst e1 < fst e2
          pure (e1, e2)
    for_ pairwise $ \((e1, p1), (e2, p2)) -> do
      zs <- lift . eover (someEnts [e1, e2]) $ do
        Unit <- query unitType
        -- TODO(sandy): constant for def size
        x <- queryDef 10 entSize
        pure (x, unchanged)
      when (length zs == 2) $ do
        let [s1, s2] = zs
            dir = normalize $ p1 - p2
            s   = s1 + s2
        lift . when (withinV2 p1 p2 s) $ do
          setEntity e1 unchanged
            { pos = Set $ p1 + dir ^* s1
            }
          setEntity e2 unchanged
            { pos = Set $ p2 - dir ^* s2
            }


initialize :: Game ()
initialize = do
  for_ [0 .. 10] $ \i -> do
    let mine = mod (round i) 2 == (0 :: Int)
    void $ createEntity newEntity
      { pos      = Just $ V2 (50 + i * 10 + bool 0 400 mine) (120 + i * 10)
      , attacks  = Just [gunAttackData]
      , entSize  = Just 7
      , acqRange = Just 125
      , speed    = Just 150
      , selected = bool Nothing (Just ()) mine
      , owner    = Just $ bool neutralPlayer mePlayer mine
      , unitType = Just Unit
      , hp       = Just $ Limit 100 100
      , commands  = Just stdWidgets
      }

  issueUnit @AttackCmd (Ent 0) (Ent 1)
  issueUnit @AttackCmd (Ent 9) (Ent 10)

  void $ createEntity newEntity
    { pos      = Just $ V2 700 300
    , attacks  = Just [gunAttackData]
    , entSize  = Just 10
    , speed    = Just 100
    , selected = Just ()
    , owner    = Just mePlayer
    , unitType = Just Unit
    , hp       = Just $ Limit 100 100
    , commands = Just $ buildCommandCenterWidget : psiStormWidget : stdWidgets
    }

  let volPos = V2 300 300
  void $ createEntity newEntity
    { pos      = Just volPos
    , gfx      = Just $ scale 0.4 $ toForm $ image "assets/volcano.png"
    , owner    = Just mePlayer
    , unitType = Just Building
    , hp       = Just $ Limit 100 100
    , gridSize = Just (10, 7)
    }

  start separateTask
  start acquireTask
  start $ volcanoPassive (volPos + V2 40 0) 0.4


volcanoPassive :: V2 -> Double -> Task ()
volcanoPassive v2 sc = do
  let size       = 20
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



acquireTask :: Task ()
acquireTask = forever $ do
  es <- lift . efor aliveEnts $ do
    with pos
    with attacks
    with acqRange
    without currentCommand
    queryEnt
  lift . for_ es $ issueInstant @AcquireCmd
  wait 0.5


update :: Time -> Game ()
update dt = do
  pumpTasks dt
  updateCommands dt

  -- death to infidels
  toKill <- efor aliveEnts $ do
    Limit health _ <- query hp
    guard $ health <= 0
    queryEnt

  for_ toKill deleteEntity


player :: Mouse -> Keyboard -> Game ()
player mouse kb = do
  curTT <- gets _lsCommandCont
  case curTT of
    Nothing -> playerNotWaiting mouse kb
    Just tt ->  do
      case tt of
        InstantCommand (GameCont f) -> do
          f ()
          unsetTT
        LocationCommand (GameCont f) ->
          when (mPress mouse buttonLeft) $ do
            f $ mPos mouse
            unless (kDown kb LeftShiftKey) unsetTT
        UnitCommand (GameCont f) ->
          when (mPress mouse buttonLeft) $ do
            msel <- getUnitAtPoint $ mPos mouse
            for_ msel $ \sel -> do
              f sel
              unless (kDown kb LeftShiftKey) unsetTT
        PlacementCommand _ (GameCont f) ->
          when (mPress mouse buttonLeft) $ do
            f $ mPos mouse ^. from centerTileScreen
            unless (kDown kb LeftShiftKey) unsetTT

  when (mPress mouse buttonRight) unsetTT


unsetTT :: Game ()
unsetTT = modify
        $ lsCommandCont .~ Nothing


playerNotWaiting :: Mouse -> Keyboard -> Game ()
playerNotWaiting mouse kb = do
  when (mPress mouse buttonLeft) $ do
    modify $ lsSelBox ?~ mPos mouse

  when (mUnpress mouse buttonLeft) $ do
    -- TODO(sandy): finicky
    mp1 <- gets _lsSelBox
    for_ mp1 $ \p1 -> do
      lPlayer <- gets _lsPlayer

      modify $ lsSelBox .~ Nothing
      let p2 = mPos mouse
          (tl, br) = canonicalizeV2 p1 p2

      -- TODO(sandy): can we use "getUnitsInSquare" instead?
      emap aliveEnts $ do
        p    <- query pos
        o    <- query owner
        Unit <- query unitType

        guard $ not $ isEnemy lPlayer o

        pure unchanged
          { selected =
              case liftV2 (<=) tl p && liftV2 (<) p br of
                True  -> Set ()
                False -> Unset
          }

  when (mPress mouse buttonRight) $ do
    sel <- getSelectedEnts
    for_ sel $ \ent ->
      issueLocation @MoveCmd ent $ mPos mouse

  allSel <- efor aliveEnts $ with selected >> query commands
  z <- for (listToMaybe allSel) $ \acts -> do
    for acts $ \act -> do
      for (cwHotkey act) $ \hk ->
        pure $ case kPress kb hk of
          True  -> Just $ cwCommand act
          False -> Nothing
  let zz = listToMaybe
         . catMaybes
         . fmap join
         . catMaybes
         $ sequence z
  for_ zz loadWaiting

  pure ()


cull :: [(V2, Form)] -> [Form]
cull = fmap (uncurry move)
     . filter (flip QT.pointInRect screenRect . fst)


draw :: Mouse -> Game [Form]
draw mouse = fmap (cull . DL.toList . fst)
           . surgery runWriterT
           $ do
  Map {..} <- gets _lsMap

  let emit a b = tell $ DL.singleton (a, b)
      screenCoords = do
        x <- [0..mapWidth]
        y <- [0..mapHeight]
        pure (x, y)

  for_ screenCoords $ \(x, y) ->
    for_ (mapGeometry x y) $ \f ->
      emit ((x + 1, y + 1) ^. centerTileScreen) f

  void . efor aliveEnts $ do
    p  <- query pos
    z  <- queryFlag selected
    o  <- queryDef neutralPlayer owner
    ut <- query unitType
    sz <- queryDef 10 entSize
    (gw, gh) <- queryDef (0, 0) gridSize

    let col = pColor o
    emit p $ group
      [ boolMonoid z $ traced' (rgb 0 1 0) $ circle $ sz + 5
      , case ut of
          Unit     -> filled col $ circle sz
          Missile  -> filled (rgb 0 0 0) $ circle 2
          Building -> filled (rgba 1 0 0 0.5)
                    $ polygon
                      [ (0,  0)  ^. centerTileScreen
                      , (gw, 0)  ^. centerTileScreen
                      , (gw, gh) ^. centerTileScreen
                      , (0,  gh) ^. centerTileScreen
                      ]
      ]

    -- debug draw
    void . optional $ do
      SomeCommand cmd <- query currentCommand
      Just (MoveCmd g@(_:_)) <- pure $ cast cmd
      Unit <- query unitType
      let ls = defaultLine { lineColor = rgba 0 1 0 0.5 }
      emit (V2 0 0) $ traced ls $ path $ p : g
      emit (last g) $ outlined ls $ circle 5

    void . optional $ do
      atts <- query attacks
      acq  <- query acqRange

      for_ atts $ \att ->
        emit p $ traced' (rgba 0.7 0 0 0.3) $ circle $ _aRange att
      emit p $ traced' (rgba 0.4 0.4 0.4 0.3) $ circle $ acq

  for_ screenCoords $ \(x, y) ->
    for_ (mapDoodads x y) $ \f ->
      emit ((x, y) ^. centerTileScreen) f

  void . efor aliveEnts $ do
    p <- query pos
    g <- query gfx
    emit p g

  -- draw placement command
  gets _lsCommandCont >>= \case
    Just (PlacementCommand World{gfx = Just g} _) ->
      emit (mPos mouse ^. from centerTileScreen . centerTileScreen) g
    _ -> pure ()

  box <- gets _lsSelBox
  for_ box $ \bpos -> do
    let (p1, p2) = canonicalizeV2 bpos $ mPos mouse
        size@(V2 w h) = p2 - p1
    emit (p1 + size ^* 0.5)
      . traced' (rgb 0 1 0)
      $ rect w h

  pure ()


main :: IO ()
main = play config (const $ run realState hooks initialize player update draw) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Typecraft"
           $ rgb 0 0 0

    hooks = Hooks
      { hookNewEnt = \e ->
          eon e (with gridSize) >>= traverse_ (const recomputeNavMesh)
      , hookDelEnt = \e -> do
          eon e (with gridSize) >>= \case
            Just _ -> do
              setEntity e delEntity
              recomputeNavMesh
            Nothing -> pure ()
      }

    realState = LocalState
          { _lsSelBox      = Nothing
          , _lsPlayer      = mePlayer
          , _lsTasks       = []
          , _lsDynamic     = mkQuadTree (20, 20) (V2 800 600)
          , _lsMap         = theMap
          , _lsNavMesh     = mapNavMesh theMap
          , _lsCommandCont = Nothing
          }

    theMap = maps M.! "rpg2k"

