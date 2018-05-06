{-# LANGUAGE NoImplicitPrelude #-}

module Overture
  ( module Types
  , module BasePrelude
  , module Game.Sequoia
  , module Game.Sequoia.Utils
  , module Control.Lens
  , module Overture
  , module Data.Ecstasy
  , module Linear
  , module Game.Sequoia.Window
  , module Control.Monad.State.Class
  , module Control.Monad.Trans.Class
  ) where

import Linear (norm, normalize, (*^), (^*), quadrance)
import Control.Lens hiding (without)
import Types
import BasePrelude hiding (group, rotate, lazy, index, uncons, loop, inRange)
import Game.Sequoia hiding (form)
import Game.Sequoia.Utils (showTrace)
import Game.Sequoia.Window (MouseButton (..))
import Data.Ecstasy hiding (get)
import qualified Data.Ecstasy as E
import Control.Monad.State.Class (get, gets, put, modify)
import Control.Monad.State (runState)
import Control.Monad.Trans.Class (lift)


canonicalizeV2 :: V2 -> V2 -> (V2, V2)
canonicalizeV2 v1@(V2 x y) v2@(V2 x' y')
    | liftV2 (<=) v1 v2 = (v1, v2)
    | liftV2 (<=) v2 v1 = (v2, v1)
    | otherwise = canonicalizeV2 (V2 x y') (V2 x' y)
canonicalizeV2 _ _ = error "impossible"


liftV2 :: (Double -> Double -> Bool) -> V2 -> V2 -> Bool
liftV2 f (V2 x y) (V2 x' y') = f x x' && f y y'
liftV2 _ _ _ = error "impossible"


recv :: (EntWorld 'FieldOf -> Maybe a) -> Query a
recv = E.get

recvMaybe :: (EntWorld 'FieldOf -> Maybe a) -> Query (Maybe a)
recvMaybe = E.getMaybe


boolMonoid :: Monoid m => Bool -> m -> m
boolMonoid = flip (bool mempty)


recvFlag
    :: (EntWorld 'FieldOf -> Maybe ())
    -> Query Bool
recvFlag = fmap (maybe False (const True)) . getMaybe


recvDef
    :: z
    -> (EntWorld 'FieldOf -> Maybe z)
    -> Query z
recvDef z = fmap (maybe z id) . getMaybe

type EntTarget world m = SystemT world m [Ent]


allEnts :: Monad m => EntTarget world m
allEnts = do
  (es, _) <- get
  pure $ Ent <$> [0 .. es - 1]

someEnts :: Monad m => [Ent] -> EntTarget world m
someEnts = pure

anEnt :: Monad m => Ent -> EntTarget world m
anEnt = pure . pure


eover
    :: ( HasWorld world
       , Monad m
       )
    => EntTarget world m
    -> (Ent -> QueryT world m (a, world 'SetterOf))
    -> SystemT world m [a]
eover t f = do
  es <- t
  fmap catMaybes $ for es $ \e -> do
    cs <- getEntity e
    mset <- lift $ unQueryT (f e) cs
    case mset of
      Just (a, setter) -> do
        setEntity e setter
        pure $ Just a
      Nothing ->  pure Nothing


maybeToUpdate :: Maybe a -> Update a
maybeToUpdate Nothing  = Unset
maybeToUpdate (Just a) = Set a


runGame
    :: (LocalState, SystemState EntWorld)
    -> SystemT EntWorld Underlying a
    -> ((LocalState, SystemState EntWorld), a)
runGame (gs, ss) m =
  let ((a, b), c) = flip runState gs $ yieldSystemT ss m
   in ((c, a), b)

evalGame
    :: (LocalState, SystemState EntWorld)
    -> SystemT EntWorld Underlying a
    -> a
evalGame = (snd .) . runGame


buttonLeft :: MouseButton
buttonLeft = ButtonExtra 0


buttonRight :: MouseButton
buttonRight = ButtonExtra 2


fi :: (Num b, Integral a) => a -> b
fi = fromIntegral


toV2 :: (Int, Int) -> V2
toV2 = uncurry V2 . (fi *** fi)


pumpTasks :: Time -> Game ()
pumpTasks dt = do
  tasks  <- lift $ gets _lsTasks
  lift . modify $ lsTasks .~ []
  tasks' <- fmap catMaybes . for tasks $ \task -> do
    z <- resume task
    pure $ case z of
      Left (Await f) -> Just $ f dt
      Right _        -> Nothing
  lift . modify $ lsTasks <>~ tasks'


start :: Task () -> Game ()
start t = lift . modify $ lsTasks %~ (t :)


wait :: Time -> Task ()
wait t | t <= 0 = pure ()
       | otherwise = do
           dt <- await
           wait $ t - dt


waitUntil :: Game Bool -> Task ()
waitUntil what = do
  fix $ \f -> do
    void await
    finished <- lift what
    unless finished f


getUnitsInRange :: V2 -> Double -> Game [(Ent, Double)]
getUnitsInRange v2 rng =
  efor $ \e -> do
    Unit <- recv unitType
    p <- recv pos
    let x = quadrance $ p - v2
    guard $ x <= rng * rng
    pure (e, sqrt x)


getUnitsInSquare :: V2 -> V2 -> Game [Ent]
getUnitsInSquare p1 p2 = do
  let (tl, br) = canonicalizeV2 p1 p2
  efor $ \e -> do
    p    <- recv pos
    Unit <- recv unitType
    guard $ liftV2 (<=) tl p && liftV2 (<) p br
    pure e


during :: Time -> (Double -> Task ()) -> Task ()
during dur f = do
  flip fix 0 $ \loop total -> do
    f $ total / dur
    dt <- await
    let total' = total + dt
    when (total' < dur) $ loop total'

