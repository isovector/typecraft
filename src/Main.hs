{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Behavior
import           Client
import           Control.Monad.Trans.Writer (WriterT (..))
import           Control.Monad.Writer.Class (tell)
import qualified Data.DList as DL
import qualified Data.Map as M
import           Game.Sequoia.Keyboard
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


acquireTask :: Ent -> Task ()
acquireTask ent = pure () -- do
  -- let refreshRate  = 0.25
  --     debounceRate = 3
  -- unitScript ent $ do
  --   wait refreshRate
  --   stuff <- lift . runQueryT ent
  --                 $ (,,,) <$> query pos
  --                         <*> query acqRange
  --                         <*> query attack
  --                         <*> query owner
  --   for_ stuff $ \(p, acq, att, o) -> do
  --     badGuys <- lift $ efor (fmap fst <$> getUnitsInRange p acq) $ do
  --       o' <- query owner
  --       guard $ isEnemy o o'
  --       (,) <$> queryEnt
  --           <*> query pos
  --     let bads = sortBy (comparing $ quadrance . (p - ) . snd) badGuys

  --     for_ (listToMaybe bads) $ \(t, tp) -> do
  --       let dir = p - tp
  --           acqPos = (normalize dir ^* _aRange att) + tp
  --       lift $ setEntity ent unchanged
  --         { target = Set $ TargetUnit t
  --         , pathing = bool Unset (Set $ Goal acqPos) $ norm dir > _aRange att
  --         }
  --       wait debounceRate


separateTask :: Task ()
separateTask = do
  dyn0 <- lift $ gets _lsDynamic
  let zones = QT.zones dyn0
      howMany = 10 :: Int

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
        x <- (,) <$> queryDef 10 entSize
                 <*> queryMaybe pathing
        pure (x, unchanged)
      when (length zs == 2) $ do
        let [(s1, _g1), (s2, _g2)] = zs
            dir = normalize $ p1 - p2
            s   = s1 + s2
        lift . when (withinV2 p1 p2 s) $ do
          setEntity e1 unchanged
            { pos = Set $ p1 + dir ^* s1
            -- , pathing = maybe Unset (\(Goal g) -> bool Keep Unset $ withinV2 g p2 s2) g1
            }
          setEntity e2 unchanged
            { pos = Set $ p2 - dir ^* s2
            -- , pathing = maybe Unset (\(Goal g) -> bool Keep Unset $ withinV2 g p1 s1) g2
            }


initialize :: Game ()
initialize = do
  for_ [0 .. 10] $ \i -> do
    let mine = mod (round i) 2 == (0 :: Int)
    ent <- createEntity newEntity
      { pos      = Just $ V2 (50 + i * 10 + bool 0 400 mine) (120 + i * 10)
      , attack   = Just gunAttackData
      , entSize  = Just 7
      , acqRange = Just 125
      , speed    = Just 150
      , selected = bool Nothing (Just ()) mine
      , owner    = Just $ bool neutralPlayer mePlayer mine
      , unitType = Just Unit
      , hp       = Just $ Limit 100 100
      , actions  = Just []
      , moveType = Just GroundMovement
      }
    start $ acquireTask ent

  void $ createEntity newEntity
    { pos      = Just $ V2 700 300
    , attack   = Just gunAttackData
    , entSize  = Just 10
    , speed    = Just 100
    , selected = Just ()
    , owner    = Just mePlayer
    , unitType = Just Unit
    , hp       = Just $ Limit 100 100
    , actions  = Just [psiStormAction]
    , moveType = Just GroundMovement
    }

  void $ createEntity newEntity
    { pos      = Just $ V2 0 0
    , owner    = Just mePlayer
    , unitType = Just Building
    , hp       = Just $ Limit 100 100
    , gridSize = Just (2, 2)
    }

  start separateTask


update :: Time -> Game ()
update dt = do
  pumpTasks dt

  orders <- efor (entsWith order) $
    (,) <$> queryEnt
        <*> query order
  for_ orders $ \(e, o) ->
    followOrder dt e $ getOrder o

  -- death to infidels
  emap aliveEnts $ do
    Unit <- query unitType
    Limit health _ <- query hp

    pure $ if health <= 0
              then delEntity
              else unchanged


player :: Mouse -> Keyboard -> Game ()
player mouse kb = do
  curTT <- gets _lsTargetType

  case curTT of
    Nothing -> playerNotWaiting mouse kb

    Just tt ->  do
      case tt of
        TargetTypeInstant (Using ent a) -> do
          start . a ent $ TargetUnit ent
          unless (kDown kb LeftShiftKey) unsetTT
        TargetTypeGround (Using ent a) ->
          when (mPress mouse buttonLeft) $ do
            start
              . a ent
              . TargetGround
              $ mPos mouse
            unless (kDown kb LeftShiftKey) unsetTT
        TargetTypeUnit (Using ent a) ->
          when (mPress mouse buttonLeft) $ do
            msel <- getUnitAtPoint $ mPos mouse
            for_ msel $ \sel -> do
              start
                . a ent
                $ TargetUnit sel
              unless (kDown kb LeftShiftKey) unsetTT

  when (mPress mouse buttonRight) unsetTT


unsetTT :: Game ()
unsetTT = modify
        $ lsTargetType .~ Nothing

isEnemy :: Player -> Player -> Bool
isEnemy = (/=)


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
    emap aliveEnts $ do
      with selected
      pure . setOrder . Ordered . MoveAction $ mPos mouse

  allSel <- efor aliveEnts $ do
    with selected
    (,) <$> queryEnt
        <*> (fmap (\(AbilityAction a) -> a) <$> query actions)

  z <- for (listToMaybe allSel) $ \(sel, acts) -> do
    for acts $ \act -> do
      for (_acHotkey act) $ \hk -> do
        case kPress kb hk of
          True  -> pure $ Just $ (Using sel $ _acTask act) <$ _acTType act
          False -> pure Nothing
  let zz = listToMaybe
         . catMaybes
         . fmap join
         . catMaybes
         $ sequence z
  modify $ lsTargetType .~ zz

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
      emit ((x, y) ^. centerTileScreen) f

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
    ( do
      g <- query pathing
      Unit <- query unitType
      let ls = defaultLine { lineColor = rgba 0 1 0 0.5 }
      emit (V2 0 0) $ traced ls $ path $ p : g
      emit (last g) $ outlined ls $ circle 5
      ) <|> pure ()

    ( do
      att  <- query attack
      acq <- query acqRange

      emit p $ traced' (rgba 0.7 0 0 0.3) $ circle $ _aRange att
      emit p $ traced' (rgba 0.4 0.4 0.4 0.3) $ circle $ acq
      ) <|> pure ()

  for_ screenCoords $ \(x, y) ->
    for_ (mapDoodads x y) $ \f ->
      emit ((x, y) ^. centerTileScreen) f

  void . efor aliveEnts $ do
    p <- query pos
    g <- query gfx
    emit p g

  box <- gets _lsSelBox
  for_ box $ \bpos -> do
    let (p1, p2) = canonicalizeV2 bpos $ mPos mouse
        size@(V2 w h) = p2 - p1
    emit (p1 + size ^* 0.5)
      . traced' (rgb 0 1 0)
      $ rect w h

  pure ()


main :: IO ()
main = play config (const $ run realState initialize player update draw) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Typecraft"
           $ rgb 0 0 0

    realState = LocalState
          { _lsSelBox     = Nothing
          , _lsPlayer     = mePlayer
          , _lsTasks      = []
          , _lsTargetType = Nothing
          , _lsDynamic    = mkQuadTree (8, 8) (V2 800 600)
          , _lsMap        = maps M.! "hoth"
          }

