{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.FRPNow.Time (delayTime)
import Game.Sequoia.Window (mousePos, mouseButtons)
-- import Game.Sequoia.Keyboard
import Overture hiding (init)
import AbilityUtils


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
  , _aRange     = 500
  , _aTask      = missile (missileEnt 100) (doDamage 30)
  }


initialize :: Game ()
initialize = do
  for_ [0 .. 10] $ \i -> do
    let mine = mod (round i) 2 == (0 :: Int)
    newEntity defEntity
      { pos      = Just $ V2 (i * 30 + bool 0 400 mine) (i * 50)
      , attack   = Just gunAttackData
      , target   = Just $ TargetUnit $ Ent $ round i + 1
      , speed    = Just 50
      , selected = bool Nothing (Just ()) mine
      , owner    = Just $ bool neutralPlayer mePlayer mine
      , unitType = Just Unit
      , hp       = Just $ Limit 100 100
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
  tasks <- fmap catMaybes . eover allEnts $ \e ->  do
    a <- recv attack
    t <- recv target
    let cooldown' = a ^. aCooldown.limVal - dt
        refresh = cooldown' < 0
        action = bool Nothing (Just $ _aTask a e t) refresh
        cooldown'' = bool cooldown' (a ^. aCooldown.limMax) refresh
    pure $ (action,) $ defEntity'
      { attack = Set $ a & aCooldown.limVal .~ cooldown''
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


player :: Mouse -> Game ()
player mouse = do
  when (mPress mouse buttonLeft) $ do
    lift $ modify $ lsSelBox ?~ mPos mouse

  when (mUnpress mouse buttonLeft) $ do
    -- TODO(sandy): finicky
    Just p1 <- lift $ gets _lsSelBox
    lPlayer <- lift $ gets _lsPlayer

    lift $ modify $ lsSelBox .~ Nothing
    let p2 = mPos mouse
        (tl, br) = canonicalizeV2 p1 p2

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


run :: N (B Element)
run = do
  clock    <- deltaTime <$> getClock
  -- keyboard <- getKeyboard

  mouseB <- do
    mb    <- mouseButtons
    oldMb <- sample $ delayTime clock (const False) mb
    mpos  <- mousePos
    pure $ getMouse mb oldMb mpos

  let realState = LocalState
        { _lsSelBox = Nothing
        , _lsPlayer = mePlayer
        , _lsTasks  = []
        }

  let init = fst $ runGame (realState, (0, defWorld)) initialize

  (game, _) <- foldmp init $ \state -> do
    -- arrs  <- sample $ arrows keyboard
    dt    <- sample clock
    mouse <- sample mouseB

    pure $ fst $ runGame state $ do
      player mouse
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
