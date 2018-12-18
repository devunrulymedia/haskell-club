{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Thrust.World.World where

import Control.Lens
import Control.Arrow
import Control.Monad.Writer
import Data.DList

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game

import Common.Redux
import Common.Renderable
import Common.Shapes.Shape
import Common.Entities.Block

import Thrust.Entities.Thruster
import Thrust.Game.GameEvent

data World = World
  { _scenery :: [ Block ]
  , _thruster :: Thruster
  }

makeLenses ''World

type Listener = Event -> World -> Events World
type Updater  = Float -> World -> Events World
type Reducer  = GameEvent -> World -> IOEvents World

instance Renderable World where
  render world = Pictures $
                   (render <$> world ^. scenery) ++
                   [render $ world ^. thruster]

drawNumber :: Int -> Int -> Int -> [ Picture ] -> [ Picture ]
drawNumber x y 0 nums = []
drawNumber x y n nums = let (nextColumn, digit) = quotRem n 10
                            currentDigit = translate (fromIntegral x) (fromIntegral y) (nums !! digit)
                            remainingDigits = drawNumber (x - 16) y nextColumn nums
                         in currentDigit : remainingDigits

integrate :: Float -> World -> World
integrate t w = thruster %~ accelerate t
                $ thruster %~ applyVelocity t
                $ w

bounce :: (Movable a, Moving a, Shaped a, Shaped b) => Float -> a -> b -> Events a
bounce el a b = case (shape b !!> shape a) of
  Nothing -> return a
  (Just pushout) -> do
    fireEvent (ThrusterCollision offset)
    return (move offset (applyImpulse reflected_vel a)) where
      vel           = velocity a
      unit_push     = normalizeV pushout
      offset        = mulSV (1 + el) pushout
      normal_proj   = (1 + el) * (vel `dotV` unit_push)
      reflected_vel = negate $ mulSV normal_proj unit_push

handleCollisions :: World -> Events World
handleCollisions w = thruster %%~ (flip $ foldM $ bounce 0) (w ^. scenery) $ w

updateWorld :: Updater
updateWorld t w = return w
              <&> thruster %~ update t
              <&> integrate t
              >>= handleCollisions

topLevelRedux :: Redux World
topLevelRedux = Redux
  { reducer  = noOp
  , listener = noOp
  , updater  = updateWorld
  }

worldRedux :: Redux World
worldRedux = compose
  [ connect thrusterRedux thruster
  , topLevelRedux
  ]
