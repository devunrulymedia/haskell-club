{-# language MultiParamTypeClasses #-}

module Redux where

import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Data.DList

data Redux w e = Redux
  { reducer :: w -> e -> IO w
  , updater ::  w -> Float -> Writer (DList e) w
  , listener :: w -> Event -> Writer (DList e) w
  }

compose :: Monad m => [a -> b -> m a] -> a -> b -> m a
compose fs a b = foldM (\x f -> f x b) a fs

fireEvent :: e -> Writer (DList e) ()
fireEvent event = tell $ singleton event

reduxListen :: Redux w e -> Event -> w -> IO w
reduxListen r e w = case runWriter $ listener r w e of
  (world, events) -> foldM (reducer r) world events

reduxUpdate :: Redux w e -> Float -> w -> IO w
reduxUpdate r t w = case runWriter $ updater r w t of
  (world, events) -> foldM (reducer r) world events
