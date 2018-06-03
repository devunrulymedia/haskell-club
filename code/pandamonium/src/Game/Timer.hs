{-# LANGUAGE TemplateHaskell #-}

module Game.Timer where

import Control.Lens
import Redux
import Game.GameEvent

data Pending = Pending Float GameEvent
             | Repeating Float Float GameEvent

data Timer = Timer
  { _elapsed :: Float
  , _pending :: [ Pending ]
  }

makeLenses ''Timer

reduceTimer :: GameEvent -> Timer -> IOEvents GameEvent Timer
reduceTimer (TimedEvent delay event) (Timer elapsed pending)
  = return $ Timer elapsed $ Pending (delay + elapsed) event : pending
reduceTimer (RepeatingEvent delay event) (Timer elapsed pending)
  = return $ Timer elapsed $ Repeating (delay + elapsed) delay event : pending
reduceTimer _ t = return t

updateEvents :: Float -> [ Pending ] -> Events GameEvent [ Pending ]
updateEvents _ [] = return []
updateEvents elapsed (current@(Pending dueOn event) : rest) = do
  rest' <- updateEvents elapsed rest
  if dueOn <= elapsed
  then do fireEvent event
          return rest'
  else return (current : rest')
updateEvents elapsed (current@(Repeating dueOn step event) : rest) = do
    rest' <- updateEvents elapsed rest
    if dueOn <= elapsed
    then do fireEvent event
            return (Repeating (dueOn + step) step event : rest')
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
