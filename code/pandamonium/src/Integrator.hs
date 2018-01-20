module Integrator where

import Graphics.Gloss.Data.Vector
import Ball
import World

integrate :: Float -> World -> World
integrate t w@World { ball = (Ball pos vel) } = w { ball = Ball (pos + mulSV t vel) vel } 
