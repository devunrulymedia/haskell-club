{-# LANGUAGE RankNTypes #-}

module Common.Controls.Button where

import Graphics.Gloss.Interface.IO.Game

import Common.Redux

-- no lenses, because of Events () fields: ghc doesn't support impredicative polymorphism
data Button = Button
  { boundKey  :: Key
  , held      :: Bool
  , onPress   :: Events ()
  , onRelease :: Events ()
  }

button :: Char -> Button
button key = Button { boundKey = Char key, held = False, onPress = return (), onRelease = return () }

keyPress :: Event -> Button -> Events Button
keyPress (EventKey key pressed _ _) button =
    if key /= (boundKey button)
  then return button
  else case pressed of
     Down -> do onPress button
                return (button { held = True})
     Up   -> do onRelease button
                return (button { held = False })
keyPress _ button = return button
