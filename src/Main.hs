{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.FRPNow.Time (delayTime)
import Game.Sequoia.Window (mousePos, mouseButtons)
import Game.Sequoia.Keyboard
import Overture hiding (init)


gameWidth :: Num t => t
gameWidth = 800


gameHeight :: Num t => t
gameHeight = 600


mePlayer :: Player
mePlayer = Player $ rgb 1 0 0


neutralPlayer :: Player
neutralPlayer = Player $ rgb 0.25 0.55 0.95


initialize :: Game ()
initialize = do
  for_ [0 .. 10] $ \i -> do
    newEntity defEntity
      { pos      = Just $ V2 (i * 50) (i * 50)
      , speed    = Just 50
      , selected = bool Nothing (Just ()) $ mod (round i) 2 == (0 :: Int)
      , owner    = Just $ bool neutralPlayer mePlayer $ mod (round i) 2 == (0 :: Int)
      , unitType = Just Unit
      }


update :: Time -> Game ()
update dt = do
  -- do walking
  emap $ do
    Unit   <- recv unitType
    p      <- recv pos
    s      <- recv speed
    Goal g <- recv pathing

    let dir = g - p
        dist = norm dir
        dx = s * dt

        shouldStop =
          case dx < dist of
            True  -> Keep
            False -> Unset

    pure defEntity'
      { pos     = Set $ p + dx *^ normalize dir
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
      p <- recv pos
      o <- recv owner
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
    p <- recv pos
    z <- recvFlag selected
    o <- recvDef neutralPlayer owner
    pure $ move p $ group
      [ boolMonoid z $ traced' (rgb 0 1 0) $ circle 10
      , filled (pColor o) $ rect 5 5
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
  keyboard <- getKeyboard

  mouseB <- do
    mb    <- mouseButtons
    oldMb <- sample $ delayTime clock (const False) mb
    mpos  <- mousePos
    pure $ getMouse mb oldMb mpos

  let realState = LocalState
        { _lsSelBox = Nothing
        , _lsPlayer = mePlayer
        }

  let init = fst $ runGame (realState, (0, defWorld)) initialize

  (game, _) <- foldmp init $ \state -> do
    arrs  <- sample $ arrows keyboard
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
