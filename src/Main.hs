{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.FRPNow.Time (delayTime)
import Game.Sequoia.Window (mousePos, mouseButtons)
import Game.Sequoia.Keyboard
import Overture hiding (init)
import AbilityUtils
import qualified Data.Map as M
import qualified Data.Set as S


gameWidth :: Num t => t
gameWidth = 800


gameHeight :: Num t => t
gameHeight = 600


mePlayer :: Player
mePlayer = Player $ rgb 1 0 0


neutralPlayer :: Player
neutralPlayer = Player $ rgb 0.25 0.55 0.95


gunAttackData :: Attack
gunAttackData = Attack
  { _aCooldown  = Limit 0 0.75
  , _aRange     = 300
  , _aTask      = missile (missileEnt 300) $ \v2 t -> do
      doDamage (Just 30) 30 v2 t
      explosion v2 1 $ \d -> scale (d + 0.01)
                           . filled (rgba 1 0 0 $ 1 - d / 2)
                           . circle
                           $ 8 + d * 3
  }



psiStormAction :: Action
psiStormAction = Action
  { _acName   = "Psi Storm"
  , _acHotkey = Just PKey
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
        . const
        . fmap ((),)
        $ performDamage dmg



initialize :: Game ()
initialize = do
  for_ [0 .. 10] $ \i -> do
    let mine = mod (round i) 2 == (0 :: Int)
    newEntity defEntity
      { pos      = Just $ V2 (i * 30 + bool 0 400 mine) (i * 50)
      , attack   = Just gunAttackData
      -- , target   = Just $ TargetUnit $ Ent $ round i + 1
      , speed    = Just 50
      , selected = bool Nothing (Just ()) mine
      , owner    = Just $ bool neutralPlayer mePlayer mine
      , unitType = Just Unit
      , hp       = Just $ Limit 100 100
      }

  void $ newEntity defEntity
    { pos      = Just $ V2 700 300
    , attack   = Just gunAttackData
    , target   = Just $ TargetGround $ V2 400 300
    , speed    = Just 100
    , selected = Just ()
    , owner    = Just mePlayer
    , unitType = Just Unit
    , hp       = Just $ Limit 100 100
    , actions  = Just [psiStormAction]
    }


moveTowards :: Time -> V2 -> Query (Bool, V2)
moveTowards dt g = do
  p <- recv pos
  s <- recv speed

  let dir = g - p
      dist = norm dir
      dx = s * dt

  pure (dx < dist, p + dx *^ normalize dir)


updateAttacks :: Time -> Game ()
updateAttacks dt = do
  entPos <- fmap M.fromList . efor $ \e -> (,) <$> pure e <*> recv pos

  tasks <- fmap catMaybes . eover allEnts $ \e ->  do
    p <- recv pos
    a <- recv attack
    t <- recv target
    let cooldown'  = a ^. aCooldown.limVal - dt
        refresh    = cooldown' < 0
        tpos       = case t of
                       TargetGround v2 -> Just v2
                       TargetUnit ent  -> M.lookup ent entPos
        target'    = bool Unset Keep $ isJust tpos
        rng        = a ^. aRange
        dst        = fmap (quadrance . (p -)) tpos
        refresh'   = refresh && maybe False (<= rng * rng) dst
        cooldown'' = bool cooldown' (a ^. aCooldown.limMax) refresh'
        action     = bool Nothing (Just $ _aTask a e t) refresh'

    pure $ (action,) $ defEntity'
      { attack = Set $ a & aCooldown.limVal .~ cooldown''
      , target = target'
      }

  for_ tasks start


update :: Time -> Game ()
update dt = do
  pumpTasks dt
  updateAttacks dt

  -- death to infidels
  emap $ do
    Unit <- recv unitType
    Limit health _ <- recv hp

    pure $ if health <= 0
              then delEntity
              else defEntity'

  -- do walking
  emap $ do
    Goal g <- recv pathing
    (notThereYet, p) <- moveTowards dt g

    let shouldStop =
          case notThereYet of
            True  -> Keep
            False -> Unset

    pure defEntity'
      { pos     = Set p
      , pathing = shouldStop
      }


player :: Mouse -> Keyboard -> Game ()
player mouse kb = do
  curTT <- lift $ gets _lsTargetType

  case curTT of
    Nothing -> playerNotWaiting mouse kb

    Just tt ->  do
      case tt of
        TargetTypeInstant (Using ent a) -> do
          start . a ent $ TargetUnit ent
          unsetTT
        TargetTypeGround (Using ent a) ->
          when (mPress mouse buttonLeft) $ do
            start
              . a ent
              . TargetGround
              $ mPos mouse
            unsetTT
        TargetTypeUnit _ ->
          error "fuck"


  when (mPress mouse buttonRight) unsetTT

unsetTT :: Game ()
unsetTT = lift
        . modify
        $ lsTargetType .~ Nothing


playerNotWaiting :: Mouse -> Keyboard -> Game ()
playerNotWaiting mouse kb = do
  when (mPress mouse buttonLeft) $ do
    lift $ modify $ lsSelBox ?~ mPos mouse

  when (mUnpress mouse buttonLeft) $ do
    -- TODO(sandy): finicky
    mp1 <- lift $ gets _lsSelBox
    for_ mp1 $ \p1 -> do
      lPlayer <- lift $ gets _lsPlayer

      lift $ modify $ lsSelBox .~ Nothing
      let p2 = mPos mouse
          (tl, br) = canonicalizeV2 p1 p2

      -- TODO(sandy): can we use "getUnitsInSquare" instead?
      emap $ do
        p    <- recv pos
        o    <- recv owner
        Unit <- recv unitType

        guard $ o == lPlayer
        pure defEntity'
          { selected =
              case liftV2 (<=) tl p && liftV2 (<) p br of
                True  -> Set ()
                False -> Unset
          }

  when (mPress mouse buttonRight) $ do
    emap $ do
      with selected
      pure defEntity'
        { pathing = Set $ Goal $ mPos mouse
        }

  allSel <- efor $ \e -> do
    with selected
    (,) <$> pure e
        <*> recv actions

  z <- for (listToMaybe allSel) $ \(sel, acts) -> do
    for acts $ \act -> do
      for (_acHotkey act) $ \hk -> do
        case kPress kb hk of
          True  -> pure $ Just $ (Using sel $ _acTask act) <$ _acTType act
          False -> pure Nothing
  let zz = join
         . join
         . join
         . listToMaybe
        $ sequence z
  lift . modify $ lsTargetType .~ zz

  pure ()


draw :: Mouse -> Game [Form]
draw mouse = do
  es <- efor $ const $ do
    p  <- recv pos
    z  <- recvFlag selected
    o  <- recvDef neutralPlayer owner
    ut <- recv unitType

    pure $ move p $ group
      [ boolMonoid z $ traced' (rgb 0 1 0) $ circle 10
      , case ut of
          Unit    -> filled (pColor o) $ rect 5 5
          Missile -> filled (rgb 0.7 0.7 0.7) $ circle 2
      ]

  exs <- efor $ const $ do
    p <- recv pos
    g <- recv gfx
    pure $ move p g

  -- draw hud
  box <- lift $ gets _lsSelBox
  let selbox =
        case box of
          Just bpos ->
            let (p1, p2) = canonicalizeV2 bpos $ mPos mouse
                size@(V2 w h) = p2 - p1
             in move p1 $ move (size ^* 0.5) $ traced' (rgb 0 1 0) $ rect w h
          Nothing -> mempty

  pure $ es
      ++ exs
      ++ [ selbox
         ]


getMouse
    :: B (MouseButton -> Bool)
    -> B (MouseButton -> Bool)
    -> B (Int, Int)
    -> B Mouse
getMouse buttons oldButtons mouse = do
  mPos     <- toV2 <$> sample mouse
  mPress   <- sample $ (\b' b z -> b' z && not (b z)) <$> buttons <*> oldButtons
  mUnpress <- sample $ (\b' b z -> b' z && not (b z)) <$> oldButtons <*> buttons
  mDown    <- sample buttons
  let mUp = fmap not mDown
  pure Mouse {..}


getKB
    :: B [Key]
    -> B [Key]
    -> B Keyboard
getKB keys oldKeys = do
  kDown     <- keys
  kLastDown <- oldKeys
  let kPress k   = elem k kDown && not (elem k kLastDown)
      kUnpress k = elem k kLastDown && not (elem k kDown)
      kPresses = S.toList $ S.fromList kDown S.\\ S.fromList kLastDown
  pure Keyboard {..}


run :: N (B Element)
run = do
  clock    <- deltaTime <$> getClock

  keyboard <- do
    kb <- getKeyboard
    oldKb <- sample $ delayTime clock [] kb
    pure $ getKB kb oldKb

  mouseB <- do
    mb    <- mouseButtons
    oldMb <- sample $ delayTime clock (const False) mb
    mpos  <- mousePos
    pure $ getMouse mb oldMb mpos

  let realState = LocalState
        { _lsSelBox     = Nothing
        , _lsPlayer     = mePlayer
        , _lsTasks      = []
        , _lsTargetType = Nothing
        }

  let init = fst $ runGame (realState, (0, defWorld)) initialize

  (game, _) <- foldmp init $ \state -> do
    -- arrs  <- sample $ arrows keyboard
    dt    <- sample clock
    kb    <- sample keyboard
    mouse <- sample mouseB

    pure $ fst $ runGame state $ do
      player mouse kb
      update dt

  pure $ do
    state <- sample game
    mouse <- sample mouseB

    pure . collage gameWidth gameHeight
         . evalGame state
         $ draw mouse


main :: IO ()
main = play config (const run) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Typecraft"
           $ rgb 0 0 0
