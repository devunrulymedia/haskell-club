{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Components.Controls where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game

import Data.ConstrainedDynamic
import Common.Components.Entity
import Common.Redux

data Button = Button
  { _boundKey  :: Key
  , _held      :: Bool
  , _onPress   :: Maybe DynEvent
  , _onRelease :: Maybe DynEvent
  } deriving Component

makeLenses ''Button

button :: Char -> Button
button key = Button { _boundKey = Char key, _held = False, _onPress = Nothing, _onRelease = Nothing }

fires :: ReduxEvent a => a -> Maybe DynEvent
fires a = Just $ toDyn a

keyPress :: Event -> Button -> Events Button
keyPress (EventKey key pressed _ _) button =
    if key /= (button ^. boundKey)
  then return button
  else case pressed of
     Down -> do traverse fireDynEvent (button ^. onPress)
                return (held .~ True $ button)
     Up   -> do traverse fireDynEvent (button ^. onRelease)
                return (held .~ False $ button)
keyPress _ button = return button

data OnAxis = Min | Neutral | Max

data Axis = Axis
  { _minButton :: Button
  , _maxButton :: Button
  , _onAxis    :: OnAxis
  } deriving Component

makeLenses ''Axis

axis :: Button -> Button -> Axis
axis min max = Axis min max Neutral

updateAxis :: Axis -> Axis
updateAxis axis = case (axis ^. minButton . held, axis ^. maxButton . held) of
  (True, False) -> onAxis .~ Min     $ axis
  (False, True) -> onAxis .~ Max     $ axis
  (_, _)        -> onAxis .~ Neutral $ axis

axisPress :: Event -> Axis -> Events Axis
axisPress event axis = return axis
                   >>= minButton %%~ keyPress event
                   >>= maxButton %%~ keyPress event
                   <&> updateAxis
