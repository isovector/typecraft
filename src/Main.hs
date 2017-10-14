{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Constants
import Control.FRPNow.Time (delayTime)
import Control.Monad.IO.Class
import Game.Sequoia
import Game.Sequoia.Color (black)
import Game.Sequoia.Keyboard
import Game.Sequoia.Window (mousePos, mouseButtons, MouseButton (ButtonLeft))
import Map (maps)
import Types


myCC :: Building
myCC = Building
  { _bPrototype = commandCenter
  , _bStats = UnitStats 1500
            $ V2 (10 * fi tileWidth)
                 (10 * fi tileHeight)
  }

drawBuilding :: Building -> Form
drawBuilding b = move (b ^. bStats . usPos)
               . toForm
               $ b ^. bPrototype . upGfx

drawPanel :: Panel a -> Form
drawPanel Panel {..} =
  move (_aabbPos _panelAABB + _aabbSize _panelAABB ^* 0.5) _panelForm

panels :: [Panel Int]
panels = [ Panel (mkPanelPos $ V2 (fi gameWidth  - fi x * (r + b))
                                  (fi gameHeight - fi y * (r + b)))
                 (8 - (y * 3 + x - 4))
                 (filled black $ rect r r)
         | x <- [1..3]
         , y <- [1..3]
         ]
  where
    b = 4
    r = 32
    mkPanelPos v2 = AABB v2 $ V2 r r

drawMap :: (Int -> Int -> [Form]) -> V2 -> Form
drawMap m cam = group
              $ [ form
                | x <- [0 .. (gameWidth  `div` tileWidth)]
                , y <- [0 .. (gameHeight `div` tileHeight)]
                , form <- m (x + d ^. _x) (y + d ^. _y)
                ]
  where
    d = floor <$> cam * V2 (1 / fi tileWidth)
                           (1 / fi tileHeight)

draw :: V2 -> V2 -> Form
draw mpos cam = group
         $ onmap
         : drawInputState mpos (PlaceBuildingState commandCenter)
         : (drawPanel <$> panels)
  where
    onmap = move (-cam)
          . group
          $ drawMap (fromJust (lookup "mindfuck" maps)) cam
          : drawBuilding myCC
          : []

alignToGrid :: V2 -> V2
alignToGrid pos = fmap fi d
                * V2 (fi tileWidth) (fi tileHeight)
  where
    d = floor <$> pos * V2 (1 / fi tileWidth)
                           (1 / fi tileHeight)

drawInputState :: V2 -> InputState -> Form
drawInputState _    NormalState = group []
drawInputState mpos (PlaceBuildingState up)
  = move (alignToGrid mpos)
  . group $
    [ toForm $ view upGfx up
    , let w = up ^. upWidth  . to fi
          h = up ^. upHeight . to fi
       in move (V2 (w / 2) (h / 2))
        . filled (rgba 0 1 0 0.5)
        $ rect w h
    ]

toV2 :: (Int, Int) -> V2
toV2 = uncurry V2 . (fi *** fi)

runGame :: N (B Element)
runGame = do
  clock      <- deltaTime <$> getClock
  keyboard   <- getKeyboard
  mouse      <- mousePos
  buttons    <- mouseButtons
  oldButtons <- sample $ delayTime clock (const False) buttons

  (game, _) <- foldmp (V2 0 0) $ \cam -> do
    arrs  <- sample $ arrows keyboard
    dt    <- sample clock
    mpos  <- toV2 <$> sample mouse
    left' <- ($ ButtonLeft) <$> sample buttons
    left  <- ($ ButtonLeft) <$> sample oldButtons

    when (left' && not left)
         . liftIO
         . print
         $ getPanelAction panels mpos

    pure $ cam + arrs ^* (10 * 16 * dt)

  pure $ do
    cam <- sample game
    mpos  <- toV2 <$> sample mouse
    pure . collage gameWidth gameHeight
         . pure
         $ draw mpos cam

main :: IO ()
main = play config (const runGame) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Typecraft"
           $ rgb 0.25 0.55 0.95

