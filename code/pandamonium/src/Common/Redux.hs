{-# LANGUAGE RankNTypes #-}

module Common.Redux where

import Data.Dynamic (Typeable)
import Data.ConstrainedDynamic
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Control.Lens
import Data.DList

class (Typeable a) => ReduxEvent a

type DynEvent = ConstrainedDynamic ReduxEvent
type EventsT m w = WriterT (DList DynEvent) m w
type Events w = EventsT Identity w
type IOEvents w = EventsT IO w

data Redux w = Redux
  { reducer :: DynEvent -> w -> IOEvents w
  , updater ::  Float -> w -> Events w
  , listener :: Event -> w -> Events w
  }

noOp :: Monad m => a -> b -> m b
noOp a b = return b

noOpRedux :: Redux w
noOpRedux = Redux
  { reducer  = noOp
  , updater  = noOp
  , listener = noOp
  }

focusM :: (ReduxEvent a, Monad m) => (a -> b -> m b) -> DynEvent -> b -> m b
focusM f = \e w -> case (fromDynamic e) of
  Just x -> f x w
  Nothing -> return w

focus :: (ReduxEvent a) => (a -> b -> b) -> DynEvent -> b -> b
focus f = \e w -> case (fromDynamic e) of
  Just x -> f x w
  Nothing -> w

fireEvent :: (ReduxEvent a, Monad m) => a -> EventsT m ()
fireEvent event = fireDynEvent (toDyn event)

fireDynEvent :: (Monad m) => DynEvent -> EventsT m ()
fireDynEvent event = tell $ singleton event

handleRemainingEvents :: Redux w -> w -> DList DynEvent -> IO w
handleRemainingEvents r w e = do (world, events) <- runWriterT $ foldM (flip $ reducer r) w e
                                 case events of
                                   Nil -> return world
                                   otherwise -> handleRemainingEvents r world events

reduxListen :: Redux w -> Event -> w -> IO w
reduxListen r e w = case runWriter $ listener r e w of
  (world, events) -> handleRemainingEvents r world events

reduxUpdate :: Redux w -> Float -> w -> IO w
reduxUpdate r t w = case runWriter $ updater r t w of
  (world, events) -> handleRemainingEvents r world events

lensing :: Functor f => Lens b b a a -> (i -> a -> f a) -> i  -> b -> f b
lensing lens f = \e -> lens %%~ (f e)

connect :: Redux a -> Lens b b a a -> Redux b
connect redux lens = Redux
  { reducer  = lensing lens (reducer redux)
  , updater  = lensing lens (updater redux)
  , listener = lensing lens (listener redux)
  }

onAll :: Traversable t => Redux a -> Redux (t a)
onAll redux = Redux
  { reducer = \e -> traverse (reducer redux e)
  , updater = \t -> traverse (updater redux t)
  , listener = \e -> traverse (listener redux e)
  }

composeHandler :: Monad m => [a -> b -> m b] -> a -> b -> m b
composeHandler fs a b = foldM (\x f -> f a x) b fs

compose :: [ Redux w ] -> Redux w
compose redii = Redux
  { reducer  = composeHandler (reducer <$> redii)
  , updater  = composeHandler (updater <$> redii)
  , listener = composeHandler (listener <$> redii)
  }
