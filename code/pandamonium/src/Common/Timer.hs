{-# LANGUAGE TemplateHaskell #-}

module Common.Timer where

import Control.Lens
import Data.Dynamic
import Data.DList
import Control.Monad.Writer

import Common.Redux

data Await = Await Float Dynamic
data Pending = Pending Float Dynamic

data Timer = Timer
  { _elapsed :: Float
  , _pending :: [ Pending ]
  }

makeLenses ''Timer

awaitEvent :: (Typeable a, Monad m) => Float -> a -> WriterT (DList Dynamic) m ()
awaitEvent delay event = fireEvent (Await delay (toDyn event))

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
  then do fireEvent event
          return rest'
  else return (current : rest')

updateTimer :: Float -> Timer -> Events Timer
updateTimer step (Timer elapsed pending) = do
  let elapsed' = step + elapsed
  pending' <- updateEvents elapsed' pending
  return $ Timer elapsed' pending'

timerRedux :: Redux Timer
timerRedux = Redux
  { reducer =  concrify reduceTimer
  , updater =  updateTimer
  , listener = noOp
  }
