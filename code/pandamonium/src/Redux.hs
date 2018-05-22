{-# language MultiParamTypeClasses #-}

module Redux where

import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Data.DList

type Events e w = Writer (DList e) w

data Redux w e = Redux
  { reducer :: e -> w -> IO w
  , updater ::  Float -> w -> Events e w
  , listener :: Event -> w -> Events e w
  }

compose :: Monad m => [a -> b -> m b] -> a -> b -> m b
compose fs a b = foldM (\x f -> f a x) b fs

fireEvent :: e -> Events e ()
fireEvent event = tell $ singleton event

reduxListen :: Redux w e -> Event -> w -> IO w
reduxListen r e w = case runWriter $ listener r e w of
  (world, events) -> foldM (flip $ reducer r) world events

reduxUpdate :: Redux w e -> Float -> w -> IO w
reduxUpdate r t w = case runWriter $ updater r t w of
  (world, events) -> foldM (flip $ reducer r) world events
