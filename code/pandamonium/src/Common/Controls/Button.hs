{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Common.Controls.Button where

import Data.ConstrainedDynamic
import Control.Lens
import Graphics.Gloss.Interface.IO.Game

import Common.Redux

data Button = Button
  { _boundKey  :: Key
  , _held      :: Bool
  , _onPress   :: Maybe DynEvent
  , _onRelease :: Maybe DynEvent
  }

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
