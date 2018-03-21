{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Collision
import Data.Data.Lens (biplate)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Writer
import Constants
import Control.FRPNow.Time (delayTime)
import Game.Sequoia
import Game.Sequoia.Utils (showTrace)
import Game.Sequoia.Color (black, red)
import Game.Sequoia.Keyboard
import Game.Sequoia.Window (mousePos, mouseButtons, MouseButton (..))
import Map (maps)
import Types hiding (left, left')
import Utils (alignToGrid)


buttonLeft :: MouseButton
buttonLeft = ButtonExtra 0

buttonRight :: MouseButton
buttonRight = ButtonExtra 2

type Game = WriterT [Command] ((->) State)

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


panels :: [Panel Command]
panels = [ Panel (mkPanelPos $ V2 (fi gameWidth  - fi x * (r + b))
                                  (fi gameHeight - fi y * (r + b)))
                 (fst $ what x y)
                 (filled black $ rect r r)
                 (snd $ what x y)
         | x :: Int <- [1..3]
         , y :: Int <- [1..3]
         ]
  where
    b = 4
    r = 32
    mkPanelPos v2 = AABB v2 $ V2 r r

    what 3 3 = (PlaceBuilding commandCenter, Just CKey)
    what 2 2 = (PlaceBuilding nothing, Just NKey)
    what _ _ = (DoNothing, Nothing)


drawMap :: (Int -> Int -> [Form]) -> V2 -> Form
drawMap m cam = group
              $ [ frm
                | x <- [0 .. (gameWidth  `div` tileWidth)]
                , y <- [0 .. (gameHeight `div` tileHeight)]
                , frm <- m (x + d ^. _x) (y + d ^. _y)
                ]
  where
    d = floor <$> cam * V2 (1 / fi tileWidth)
                           (1 / fi tileHeight)


getBuildings :: State -> [Building]
getBuildings s = s ^.. biplate

draw :: V2 -> State -> Form
draw mpos state = group $
         ( onmap
         : drawInputState mpos (state ^. sLocalState . lsInputState)
         : (drawPanel <$> panels)
         ) ++
         ( fmap (move (-cam)) $
           (state ^. sLocalState . lsDebugVis)
           : debugDrawQuad tree
         -- ++ debugDrawConnectivity tree
         )
  where
    tree = buildQuadTree (100, 100) $ getBuildings state
    cam = state ^. sLocalState . lsCamera
    onmap = move (-cam)
          . group
          $ drawMap (fromJust (lookup "mindfuck" maps)) cam
          : (drawBuilding <$> getBuildings state)


drawInputState :: V2 -> InputState -> Form
drawInputState _    NormalState = group []
drawInputState _    (DebugVisPathingState _) = group []
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

  (game, _) <- foldmp defState $ \state -> do
    arrs  <- sample $ arrows keyboard
    dt    <- sample clock
    mpos  <- toV2 <$> sample mouse
    press <- sample $ (\b' b z -> b' z && not (b z)) <$> buttons <*> oldButtons

    hks <- fmap (mapMaybe id) . for panels $ \p ->
      fmap join . for (_panelHotKey p) $ \hk -> do
        down <- sample $ isDown keyboard hk
        if down
           then pure . Just $  _panelAction p
           else pure Nothing

    pure $
      (runUpdateGame state $ updateGame mpos press dt >> tell hks)
      & sLocalState . lsCamera %~ (+ arrs ^* (10 * 16 * dt))

  pure $ do
    state <- sample game
    mpos  <- toV2 <$> sample mouse
    pure . collage gameWidth gameHeight
         . pure
         $ draw mpos state


updateGame :: V2 -> (MouseButton -> Bool) -> Time -> Game ()
updateGame mpos' press _ = do
  s <- ask
  let cam = s ^. sLocalState . lsCamera
      mpos = mpos' + cam

  case s ^. sLocalState . lsInputState of
    NormalState -> do
      when (press buttonLeft)
          . tell
          . maybeToList
          $ getPanelAction panels mpos
      when (press buttonRight)
          . tell
          . pure
          $ DebugVisStartPathing mpos

    PlaceBuildingState pt -> do
      when (press buttonLeft)
          . tell
          . pure
          . ConfirmBuilding pt
          $ alignToGrid mpos

    DebugVisPathingState src -> do
      when (press buttonRight)
          . tell
          . pure
          $ DebugVisPathing src mpos



runUpdateGame :: State -> Game () -> State
runUpdateGame s w
  = ($ s)
  . appEndo
  . foldMap (Endo . runCommand)
  . ($ s)
  $ execWriterT w



runCommand :: Command -> State -> State
runCommand DoNothing           = id
runCommand (PlaceBuilding pt)  = sLocalState . lsInputState .~ PlaceBuildingState pt
runCommand (DebugVisStartPathing src)  = sLocalState . lsInputState .~ DebugVisPathingState src
runCommand (ConfirmBuilding pt pos) = \s ->
  s & sLocalState . lsInputState .~ NormalState
    & sGameState . gsPlayers . ix (s ^. sLocalState . lsPlayer) . pOwned . poBuildings %~
      \bs -> (Building { _bPrototype = pt, _bStats = prototypeToStats pos pt} ) : bs
runCommand (DebugVisPathing src dst) = \s ->
  s & sLocalState . lsInputState .~ NormalState
    & sLocalState . lsDebugVis .~ debugDrawLines red (join (maybeToList $ pathfind (buildQuadTree (100, 100) $ getBuildings s) src dst))


main :: IO ()
main = play config (const runGame) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Typecraft"
           $ rgb 0.25 0.55 0.95

