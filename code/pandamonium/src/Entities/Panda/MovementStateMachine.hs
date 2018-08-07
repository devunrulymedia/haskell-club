{-# LANGUAGE TemplateHaskell #-}

module Entities.Panda.MovementStateMachine where

import Control.Lens
import Graphics.Gloss.Data.Vector
import Game.GameEvent

type Fuel = Float
data Direction = DLeft | DRight deriving (Show, Eq)

pushOff :: Direction -> Float -> Float
pushOff DLeft x = x
pushOff DRight x = (-x)

invert :: Direction -> Direction
invert DLeft = DRight
invert DRight = DLeft

data MovementState = Grounded
                   | Jumping Fuel
                   | Falling
                   | WallHugging Direction
                   | WallJumping Direction Fuel
                   deriving (Show, Eq)
