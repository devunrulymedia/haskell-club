{-# LANGUAGE ScopedTypeVariables #-}

module Panda2.Entities.Panda where

import Data.Maybe
import Control.Lens
import Graphics.Gloss (Picture)
import Common.Components
import Common.Monad
import Common.Controls.Axis

import Panda2.Controller

panda :: [ Picture ] -> EntityId -> Entity
panda pandas = entity
           <-: Position (20, 20)
           <-: Sprite (head pandas)
           <-: PlayerIndex 1

movePanda :: Controller -> Entity -> Entity
movePanda c e = fromMaybe e $ do
  player :: PlayerIndex <- extract e
  (Position (x, y)) <- extract e
  ifMaybe (player == c ^. playerIndex) $ do
    case (c ^. horizontal . onAxis) of
      Min -> return $ e <-+ Position (x - 1, y)
      Neutral -> Nothing
      Max -> return $ e <-+ Position (x + 1, y)

movePandas :: (Controller, [ Entity ]) -> (Controller, [ Entity ])
movePandas (controller, entities) = (controller, movePanda controller <$> entities)
