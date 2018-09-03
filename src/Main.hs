{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Ecstasy.Types (Ent (..))
import           Art
import           Behavior
import           Client
import           Control.Monad.Trans.Writer.Strict (WriterT (..))
import           Control.Monad.Writer.Class (tell)
import qualified Data.DList as DL
import           Data.Ecstasy.Types (Hooks (..))
import qualified Data.Map as M
import           GameData
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


initialize :: Game ()
initialize = do
  for_ [0 .. 10] $ \i -> do
    let mine = mod (round i) 2 == (0 :: Int)
    void $ createEntity marineProto
      { pos      = Just $ V2 (50 + i * 10 + bool 0 400 mine) (120 + i * 10)
      , selected = bool Nothing (Just ()) mine
      , owner    = Just $ bool neutralPlayer mePlayer mine
      }

  issueUnit @AttackCmd () (Ent 0) (Ent 1)
  issueUnit @AttackCmd () (Ent 9) (Ent 10)

  void $ createEntity garethProto
    { pos      = Just $ V2 450 400
    , owner    = Just mePlayer
    }

  void $ createEntity mineralsProto
    { pos      = Just $ V2 (tileWidth * 16) (tileHeight * 15)
    }

  void $ createEntity newEntity
    { pos      = Just $ V2 500 400
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
    , commands = Just [volcanoPassiveWidget]
    }

  void $ start acquireTask





acquireTask :: Task ()
acquireTask = forever $ do
  es <- lift . efor aliveEnts $ do
    with pos
    with attacks
    with acqRange
    without currentCommand
    queryEnt
  lift . for_ es $ issueInstant @AcquireCmd ()
  wait 0.5


update :: Time -> Game ()
update dt = do
  pumpTasks dt
  updateCommands dt

  emap aliveEnts $ do
    pure unchanged
      { lifetime = Modify (+ dt)
      }

  emap (entsWith art) $ do
    a <- query art
    pure unchanged
      { art = Set $ a & aTime +~ dt
      }

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
        InstantCommand (GameCont _ f) -> do
          f ()
          unsetTT
        LocationCommand (GameCont _ f) ->
          when (mPress mouse buttonLeft) $ do
            f $ mPos mouse
            unless (kDown kb LeftShiftKey) unsetTT
        UnitCommand (GameCont _ f) ->
          when (mPress mouse buttonLeft) $ do
            msel <- getUnitAtPoint $ mPos mouse
            for_ msel $ \sel -> do
              f sel
              unless (kDown kb LeftShiftKey) unsetTT
        PlacementCommand (GameCont _ f) ->
          when (mPress mouse buttonLeft) $ do
            f $ mPos mouse ^. from centerTileScreen
            unless (kDown kb LeftShiftKey) unsetTT
        PassiveCommand _ ->
          error "someone tried to start a passive"

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
        query unitType >>= guard . (/= Missile)

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
      whenM (hasWidget @MoveCmd ent)
        . issueLocation @MoveCmd () ent
        $ mPos mouse

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
    sz <- queryDef 10 entSize
    emit p . boolMonoid z
           . traced' (rgb 0 1 0)
           . circle
           $ sz + 5

  void . efor aliveEnts $ do
    without gfx
    without art
    p  <- query pos
    o  <- queryDef neutralPlayer owner
    ut <- query unitType
    sz <- queryDef 10 entSize
    (gw, gh) <- queryDef (0, 0) gridSize

    let col = pColor o
    emit p $
      case ut of
        Unit     -> filled col $ circle sz
        Missile  -> filled (rgb 0 0 0) $ circle 2
        Building -> filled (rgba 1 0 0 0.5)
                  $ polygon
                    [ (0,  0)  ^. centerTileScreen
                    , (gw, 0)  ^. centerTileScreen
                    , (gw, gh) ^. centerTileScreen
                    , (0,  gh) ^. centerTileScreen
                    ]

  for_ screenCoords $ \(x, y) ->
    for_ (mapDoodads x y) $ \f ->
      emit ((x, y) ^. centerTileScreen) f

  void . efor aliveEnts $ do
    without art
    p <- query pos
    g <- query gfx
    emit p g

  void . efor aliveEnts $ do
    p <- query pos
    a <- query art
    d <- query lastDir
    emit p . bool (scaleXY (-1) 1)
                  id
                  (dot d (V2 1 0) >= 0)
           $ drawArt a Nothing

  -- draw placement command
  gets _lsCommandCont >>= \case
    Just (PlacementCommand gc) ->
      case cast @_ @(GameCont BuildCmd (Int, Int)) gc of
        Just (GameCont World{gfx = Just g} _) ->
          emit (mPos mouse ^. from centerTileScreen . centerTileScreen) g
        _ -> pure ()
    _ -> pure ()

  box <- gets _lsSelBox
  for_ box $ \bpos -> do
    let (p1, p2) = canonicalizeV2 bpos $ mPos mouse
        size@(V2 w h) = p2 - p1
    emit (p1 + size ^* 0.5)
      . traced' (rgb 0 1 0)
      $ rect w h

  -- debug draw
  void . efor aliveEnts $ do
    p  <- query pos
    void . optional $ do
      SomeCommand cmd <- query currentCommand
      Just (MoveCmd g@(_:_) _ _) <- pure $ cast cmd
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

  pure ()


-- TODO(sandy): ;
main :: IO ()
main = play config (const $ run realState hooks initialize player update draw) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Typecraft"
           $ rgb 0 0 0

    hooks = Hooks
      { hookNewEnt = \e -> do
          eon e (with gridSize) >>= traverse_ (const recomputeNavMesh)
          eon e (with animBundle) >>= traverse_ (const $ playAnim e [AnimIdle])
      , hookDelEnt = \e -> do
          everything <- getEntity e
          for_ (gridSize everything) . const . start $ lift recomputeNavMesh
          for_ (activePassives everything)
            . traverse_
            $ \(SomeCommand (a :: a)) -> endCommand @a e $ Just a
      }

    realState = LocalState
          { _lsSelBox      = Nothing
          , _lsPlayer      = mePlayer
          , _lsTasks       = mempty
          , _lsNewTasks    = []
          , _lsTaskId      = 0
          , _lsDynamic     = mkQuadTree (20, 20) (V2 800 600)
          , _lsMap         = theMap
          , _lsNavMesh     = mapNavMesh theMap
          , _lsCommandCont = Nothing
          }

    theMap = maps M.! "rpg2k"

