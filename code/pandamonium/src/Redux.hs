{-# LANGUAGE RankNTypes #-}

module Redux where

import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Control.Lens
import Data.DList

type Events e w = Writer (DList e) w

data Redux w e = Redux
  { reducer :: e -> w -> IO w
  , updater ::  Float -> w -> Events e w
  , listener :: Event -> w -> Events e w
  }

noOp :: Monad m => a -> b -> m b
noOp a b = return b

noOpRedux :: Redux e w
noOpRedux = Redux
  { reducer  = noOp
  , updater  = noOp
  , listener = noOp
  }

fireEvent :: e -> Events e ()
fireEvent event = tell $ singleton event

reduxListen :: Redux w e -> Event -> w -> IO w
reduxListen r e w = case runWriter $ listener r e w of
  (world, events) -> foldM (flip $ reducer r) world events

reduxUpdate :: Redux w e -> Float -> w -> IO w
reduxUpdate r t w = case runWriter $ updater r t w of
  (world, events) -> foldM (flip $ reducer r) world events

lensing :: Functor f => Lens b b a a -> (e -> a -> f a) -> e -> b -> f b
lensing lens f = \e -> lens %%~ (f e)

connect :: Redux a e -> Lens b b a a -> Redux b e
connect redux lens = Redux
  { reducer  = lensing lens (reducer redux)
  , updater  = lensing lens (updater redux)
  , listener = lensing lens (listener redux)
  }

compose :: [Redux w e] -> Redux w e
compose redii = Redux
  { reducer  = compose' (reducer <$> redii)
  , updater  = compose' (updater <$> redii)
  , listener = compose' (listener <$> redii)
  } where
    compose' :: Monad m => [a -> b -> m b] -> a -> b -> m b
    compose' fs a b = foldM (\x f -> f a x) b fs
