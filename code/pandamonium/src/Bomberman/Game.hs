module Bomberman.Game where

import Control.Lens
import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Event)

import Common.Redux
import Common.Components
import Common.Shapes.Shape

moveHorizontally :: a -> Axis -> Velocity
moveHorizontally _ axis = case axis ^. onAxis of
  Min     -> Velocity (-200, 0)
  Neutral -> Velocity (0, 0)
  Max     -> Velocity (200, 0)

updateEntity :: Float -> Entity -> Events Entity
updateEntity t e = return e
               <&> update1 moveHorizontally t

listenEntity :: Event -> Entity -> Events Entity
listenEntity event entity = return entity
                        >>= updateM1 axisPress event

player :: Entity
player = entity
     <-+ Position (100, 100)
     <-+ circle (0, 0) 50
     <-+ yellow
     <-+ axis (button 'z') (button 'x')

wall :: Entity
wall = entity
   <-+ rectangleV (-200, -200) (400, 200)
   <-+ white
   <-+ Immovable

customRedux :: Redux World
customRedux = Redux
  { updater  = lensing entities (onEach updateEntity)
  , listener = lensing entities (onEach listenEntity)
  , reducer  = noOp
  }

initialise :: Events ()
initialise = do
  spawn player
  spawn wall

bombermanGame :: IO World
bombermanGame = reduxDo bombermanRedux (newWorld coloredShape) initialise

bombermanRedux :: Redux World
bombermanRedux = compose [ customRedux, physics, lifecycle ]
