{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Common.Controls.Axis where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game

import Common.Redux
import Common.Controls.Button

data OnAxis = Min | Neutral | Max

data Axis = Axis
  { _minButton :: Button
  , _maxButton :: Button
  , _onAxis    :: OnAxis
  }

makeLenses ''Axis

axis :: Button -> Button -> Axis
axis min max = Axis min max Neutral

updateAxis :: Axis -> Axis
updateAxis axis = case (held $ axis ^. minButton, held $ axis ^. maxButton) of
  (True, False) -> onAxis .~ Min     $ axis
  (False, True) -> onAxis .~ Max     $ axis
  (_, _)        -> onAxis .~ Neutral $ axis

axisPress :: Event -> Axis -> Events Axis
axisPress event axis = return axis
                   >>= minButton %%~ keyPress event
                   >>= maxButton %%~ keyPress event
                   <&> updateAxis
