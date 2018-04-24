{-# language MultiParamTypeClasses #-}

module Redux where

import Control.Monad.Writer
import Data.DList

redux :: (Float -> a -> Writer (DList e) a) -> (a -> e -> IO a) -> Float -> a -> IO a
redux runState runEvent t a = case runWriter $ runState t a of
  (world, events) -> foldM runEvent world events
