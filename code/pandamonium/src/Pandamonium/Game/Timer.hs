{-# LANGUAGE TemplateHaskell #-}

module Pandamonium.Game.Timer where

import Control.Lens
import Common.Redux

data DueIn a = DueIn Float a
data DueAt a = DueAt Float a

class TimedEvent a where
  timed :: a -> Maybe (DueIn a)

data Timer a = Timer
  { _elapsed :: Float
  , _pending :: [ DueAt a ]
  }

makeLenses ''Timer

reduceTimer :: (TimedEvent a) => a -> Timer a -> IOEvents a (Timer a)
reduceTimer event timer = reduceTimer' (timed event) where
  reduceTimer' (Just (DueIn delay action)) = return $ (pending %~ (DueAt (delay + (timer ^. elapsed)) action :) $ timer)
  reduceTimer' Nothing = return timer

updateEvents :: (TimedEvent a) => Float -> [ DueAt a ] -> Events a [ DueAt a ]
updateEvents _ [] = return []
updateEvents elapsed (current@(DueAt dueAt event) : rest) = do
  rest' <- updateEvents elapsed rest
  if dueAt <= elapsed
  then do fireEvent event
          return rest'
  else return (current : rest')

updateTimer :: (TimedEvent a) => Float -> (Timer a) -> Events a (Timer a)
updateTimer step (Timer elapsed pending) = do
  let elapsed' = step + elapsed
  pending' <- updateEvents elapsed' pending
  return $ Timer elapsed' pending'

timerRedux :: (TimedEvent a) => Redux (Timer a) a
timerRedux = Redux
  { reducer =  reduceTimer
  , updater =  updateTimer
  , listener = noOp
  }
