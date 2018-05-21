{-# language MultiParamTypeClasses #-}

module Redux where

import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Data.DList

data Redux w e = Redux
  { reducer :: w -> e -> IO w
  , updater ::  w -> Float -> Writer (DList e) w
  , listener :: Event -> w -> [e]
  }

compose :: Monad m => [a -> b -> m a] -> a -> b -> m a
compose fs a b = foldM (\x f -> f x b) a fs

composeListeners :: [Event -> w -> [e]] -> Event -> w -> [e]
composeListeners ls e w = ls >>= (\l -> l e w)

reduxListen :: Redux w e -> Event -> w -> IO w
reduxListen r e w = foldM (reducer r) w (listener r e w)
reduxUpdate :: Redux w e -> Float -> w -> IO w
reduxUpdate r t w = case runWriter $ updater r w t of
  (world, events) -> foldM (reducer r) world events
