module World.GameEvent where

data GameEvent = PointScored Int
               | Quit

class GameEvents a where
  handleEvent :: GameEvent -> a -> a
