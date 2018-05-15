{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Client
  ( run
  , gameWidth
  , gameHeight
  ) where

import           Control.FRPNow.Time (delayTime)
import qualified Data.Set as S
import           Game.Sequoia.Keyboard
import           Game.Sequoia.Window (mousePos, mouseButtons)
import           Overture hiding (init)


gameWidth :: Num t => t
gameWidth = 800


gameHeight :: Num t => t
gameHeight = 600


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


run :: LocalState
    -> Game ()
    -> (Mouse -> Keyboard -> Game ())
    -> (Time -> Game ())
    -> (Mouse -> Game [Form])
    -> N (B Element)
run realState initialize player update draw = do
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

  let world = defStorage
              { pos = VTable vgetPos vsetPos
              }
      init = fst $ runGame (realState, (0, world)) initialize

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

