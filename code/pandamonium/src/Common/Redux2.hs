{-# LANGUAGE RankNTypes #-}

module Common.Redux2 where

-- adding a comment because why not?

import Data.Dynamic
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Control.Lens
import Data.DList

type Events w = Writer (DList Dynamic) w
type IOEvents w = WriterT (DList Dynamic) IO w

data Redux w = Redux
  { reducer :: Dynamic -> w -> IOEvents w
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

fireEvent :: Monad m => Dynamic -> WriterT (DList Dynamic) m ()
fireEvent event = tell $ singleton event

handleRemainingEvents :: Redux w -> w -> DList Dynamic -> IO w
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

lensing :: Functor f => Lens b b a a -> (i -> a -> f a) -> i -> b -> f b
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

compose :: [ Redux w ] -> Redux w
compose redii = Redux
  { reducer  = compose' (reducer <$> redii)
  , updater  = compose' (updater <$> redii)
  , listener = compose' (listener <$> redii)
  } where
    compose' :: Monad m => [a -> b -> m b] -> a -> b -> m b
    compose' fs a b = foldM (\x f -> f a x) b fs
