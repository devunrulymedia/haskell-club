module Pandamonium.Entities.Panda.MovementStateMachine where

import Control.Lens
import Graphics.Gloss.Data.Vector
import Pandamonium.Game.GameEvent

data Direction = DLeft | DRight deriving (Show, Eq)

pushOff :: Direction -> Float -> Float
pushOff DLeft x = x
pushOff DRight x = (-x)

invert :: Direction -> Direction
invert DLeft = DRight
invert DRight = DLeft

data MovementState = Grounded
                   | Airborne
                   | WallHugging Direction
                   deriving (Show, Eq)
