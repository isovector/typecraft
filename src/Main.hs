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


update :: Time -> Game ()
update dt = do
  emap $ do
    p      <- get pos
    s      <- get speed
    Goal g <- get pathing

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


player :: V2 -> (MouseButton -> Bool) -> Game ()
player mpos onDown = do
  when (onDown buttonRight) $ do
    emap $ do
      with selected
      pure defEntity'
        { pathing = Set $ Goal mpos
        }

  pure ()


draw :: V2 -> Game Form
draw _ = do
  es <- efor $ const $ (,)
                   <$> get pos
                   <*> getFlag selected
  pure $ group $ es <&> \(p, z) ->
    move p $ filled (let c = bool 0 1 z in rgb c c c) $ rect 5 5



initialize :: Game ()
initialize = do
  for_ [0 .. 10] $ \i -> do
    newEntity defEntity
      { pos     = Just $ V2 (i * 50) (i * 50)
      , pathing = Just $ Goal $ V2 400 300
      , speed   = Just 50
      , selected = Just ()
      }



run :: N (B Element)
run = do
  clock      <- deltaTime <$> getClock
  keyboard   <- getKeyboard
  mouse      <- mousePos
  buttons    <- mouseButtons
  oldButtons <- sample $ delayTime clock (const False) buttons

  let init = fst $ runGame (0, defWorld) initialize

  (game, _) <- foldmp init $ \state -> do
    arrs  <- sample $ arrows keyboard
    dt    <- sample clock
    mpos  <- toV2 <$> sample mouse
    press <- sample $ (\b' b z -> b' z && not (b z)) <$> buttons <*> oldButtons

    pure $ fst $ runGame state $ do
      player mpos press
      update dt

  pure $ do
    state <- sample game
    mpos  <- toV2 <$> sample mouse

    pure . collage gameWidth gameHeight
         . pure
         . snd
         . runGame state
         $ draw mpos


main :: IO ()
main = play config (const run) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Typecraft"
           $ rgb 0.25 0.55 0.95
