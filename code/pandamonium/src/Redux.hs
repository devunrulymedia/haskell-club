{-# language MultiParamTypeClasses #-}

module Redux where

import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Data.DList

data Redux w e = Redux
  { reducer :: w -> e -> IO w
  , updater :: Float -> w -> Writer (DList e) w
  , listener :: Event -> w -> Maybe e
  }

reduxListen :: Redux w e -> Event -> w -> IO w
reduxListen r e w = maybe (pure w) ((reducer r) w) ((listener r) e w)
reduxUpdate :: Redux w e -> Float -> w -> IO w
reduxUpdate r t w = case runWriter $ (updater r) t w of
  (world, events) -> foldM (reducer r) world events
