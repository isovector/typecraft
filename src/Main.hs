{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude hiding (group)
import Game.Sequoia
import Game.Sequoia.Keyboard


gameWidth :: Int
gameWidth = 800

gameHeight :: Int
gameHeight = 600

runGame :: N (B Element)
runGame = do
  clock <- getClock
  (game, _) <- foldmp 0 $ \n -> do
    dt <- sample $ deltaTime clock
    pure $ n + 1

  pure $ do
    n <- sample game
    pure . collage gameWidth gameHeight
         . pure
         $ group []

main :: IO ()
main = play config (const runGame) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "IWMAG"
           $ rgb 0.8 0.8 0.8

