module Pandamonium.Game.Score where

import Graphics.Gloss
import Common.Redux
import Common.Renderable

import Pandamonium.Game.GameEvent

data Score = Score Int Vector [ Picture ]

drawNumber :: Float -> Float -> Int -> [ Picture ] -> [ Picture ]
drawNumber x y 0 nums = []
drawNumber x y n nums = let (nextColumn, digit) = quotRem n 10
                            currentDigit = translate x y (nums !! digit)
                            remainingDigits = drawNumber (x - 16) y nextColumn nums
                         in currentDigit : remainingDigits

instance Renderable Score where
  render (Score score (x, y) chars) = Pictures $ drawNumber x y score chars

increaseScore :: GameEvent -> Score -> IOEvents Score
increaseScore (PointsScored x) (Score score pos chars) = return $ Score (score + x) pos chars
increaseScore _ score = return score

scoreRedux :: Redux Score
scoreRedux = noOpRedux { reducer = focus increaseScore }
