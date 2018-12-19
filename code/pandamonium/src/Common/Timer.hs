{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Common.Timer where

import Control.Lens
import Data.Dynamic (Typeable)
import Data.ConstrainedDynamic
import Data.DList
import Control.Monad.Writer

import Common.Redux

data Await = Await Float (Events ()) deriving ReduxEvent
data Pending = Pending Float (Events ()) deriving ReduxEvent

data Timer = Timer
  { _elapsed :: Float
  , _pending :: [ Pending ]
  }

newTimer = Timer 0 []

makeLenses ''Timer

await :: (ReduxEvent a) => Float -> a -> DynEvent
await delay action = toDyn (Await delay (fireEvent action))

awaitAction :: Float -> Events () -> Events ()
awaitAction delay action = fireEvent (Await delay action)

reduceTimer :: Await -> Timer -> IOEvents Timer
reduceTimer (Await delay action) timer = return $ (pending %~ (Pending (delay + (timer ^. elapsed)) action :) $ timer)

-- if this were a queue, we wouldn't need to iterate over all the events every cycle
-- however, that iteration is cheap and we don't currently expect many timed events
-- to be pending at any given time. If that changes, we should consider changing
-- this data structure then.
updateEvents :: Float -> [ Pending ] -> Events [ Pending ]
updateEvents _ [] = return []
updateEvents elapsed (current@(Pending dueAt event) : rest) = do
  rest' <- updateEvents elapsed rest
  if dueAt <= elapsed
  then do event
          return rest'
  else return (current : rest')

updateTimer :: Float -> Timer -> Events Timer
updateTimer step (Timer elapsed pending) = do
  let elapsed' = step + elapsed
  pending' <- updateEvents elapsed' pending
  return $ Timer elapsed' pending'

timerRedux :: Redux Timer
timerRedux = Redux
  { reducer =  focusM reduceTimer
  , updater =  updateTimer
  , listener = noOp
  }
