{-# LANGUAGE TemplateHaskell #-}

module Pandamonium.Game.Timer where

import Control.Lens
import Pandamonium.Redux
import Pandamonium.Game.GameEvent

data Pending = Pending Float GameEvent

data Timer = Timer
  { _elapsed :: Float
  , _pending :: [ Pending ]
  }

makeLenses ''Timer

reduceTimer :: GameEvent -> Timer -> IOEvents GameEvent Timer
reduceTimer (TimedEvent delay event) (Timer elapsed pending)
  = return $ Timer elapsed $ Pending (delay + elapsed) event : pending
reduceTimer _ t = return t

updateEvents :: Float -> [ Pending ] -> Events GameEvent [ Pending ]
updateEvents _ [] = return []
updateEvents elapsed (current@(Pending dueOn event) : rest) = do
  rest' <- updateEvents elapsed rest
  if dueOn <= elapsed
  then do fireEvent event
          return rest'
  else return (current : rest')

updateTimer :: Float -> Timer -> Events GameEvent Timer
updateTimer step (Timer elapsed pending) = do
  let elapsed' = step + elapsed
  pending' <- updateEvents elapsed' pending
  return $ Timer elapsed' pending'

timerRedux :: Redux Timer GameEvent
timerRedux = Redux
  { reducer =  reduceTimer
  , updater =  updateTimer
  , listener = noOp
  }
