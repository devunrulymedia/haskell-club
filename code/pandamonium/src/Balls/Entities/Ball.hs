{-# LANGUAGE TemplateHaskell #-}

module Balls.Entities.Ball where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char))
import Graphics.Gloss (Vector, Color, color, yellow)

import Common.Redux
import Common.Renderable
import Common.Shapes.Shape


data Ball = Ball
 { _mass :: Float
 , _radius :: Float
 , _pos :: Vector
 , _vel :: Vector
 , _col :: Color
}

makeLenses ''Ball

instance Shaped Ball where
  shape ball = circle (ball ^. pos) (ball ^. radius)

instance Renderable Ball where
  render ball = color (ball ^. col) $ render (shape ball)

zag :: Event -> Ball -> Events Ball
zag (EventKey (Char 'z') true _ _) ball = return $ pos %~ (\x -> x - (10, 0)) $ ball
zag (EventKey (Char 'x') true _ _) ball = return $ pos %~ (+ (10, 0)) $ ball
zag (EventKey (Char 'm') true _ _) ball = do
  fireEvent yellow
  return ball
zag _ ball = return ball

enhue :: Color -> Ball -> IOEvents Ball
enhue c ball = return $ col .~ c $ ball

ballRedux :: Redux Ball
ballRedux = Redux
  { updater = noOp
  , listener = zag
  , reducer = concrify enhue
  }
