module GameEvent where

data GameEvent = PointScored Int

class GameEvents a where
  handleEvent :: GameEvent -> a -> a
