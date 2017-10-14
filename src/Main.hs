{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.FRPNow.Time (delayTime)
import Control.Monad.IO.Class
import Game.Sequoia
import Game.Sequoia.Color (black)
import Game.Sequoia.Keyboard
import Game.Sequoia.Window (mousePos, mouseButtons, MouseButton (ButtonLeft))
import Types


drawPanel :: Panel a -> Form
drawPanel Panel {..} = move (aabbPos panelAABB + aabbSize panelAABB ^* 0.5) panelForm

gameWidth :: Int
gameWidth = 800

gameHeight :: Int
gameHeight = 600

panels :: [Panel Int]
panels = [ Panel (mkPanelPos $ V2 (fromIntegral gameWidth  - fromIntegral x * (r + b))
                                  (fromIntegral gameHeight - fromIntegral y * (r + b)))
                 (8 - (y * 3 + x - 4))
                 (filled black $ rect r r)
         | x <- [1..3]
         , y <- [1..3]
         ]
  where
    b = 4
    r = 32
    mkPanelPos v2 = AABB v2 $ V2 r r

draw :: Form
draw = group $ drawPanel <$> panels

toV2 :: (Int, Int) -> V2
toV2 = uncurry V2 . (fromIntegral *** fromIntegral)

runGame :: N (B Element)
runGame = do
  clock   <- deltaTime <$> getClock
  mouse   <- mousePos
  buttons <- mouseButtons
  oldButtons <- sample $ delayTime clock (const False) buttons

  (game, _) <- foldmp 0 $ \n -> do
    dt <- sample clock
    mpos <- toV2 <$> sample mouse
    left' <- ($ ButtonLeft) <$> sample buttons
    left  <- ($ ButtonLeft) <$> sample oldButtons

    when (left' && not left)
         . liftIO
         . print
         $ getPanelAction panels mpos

    pure $ n + 1

  pure $ do
    n <- sample game
    pure . collage gameWidth gameHeight
         . pure
         $ draw

main :: IO ()
main = play config (const runGame) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "IWMAG"
           $ rgb 0.8 0.8 0.8

