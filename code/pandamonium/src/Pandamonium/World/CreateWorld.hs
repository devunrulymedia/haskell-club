module Pandamonium.World.CreateWorld where

import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Key (Char), Vector)

import Common.Redux
import Common.Entities.Spawner
import Common.Entities.Block
import Common.Entities.Entity

import Pandamonium.Entities.Panda
import Pandamonium.Entities.Coin
import Pandamonium.Entities.EntityTypes
import Pandamonium.World.Assets
import Pandamonium.Systems.Controller
import Pandamonium.World.World
import Pandamonium.World.Stage

pandaAtLocation :: Assets -> Vector -> Panda
pandaAtLocation assets loc = mkPanda (pandas assets) loc (withKeys (Char 'z') (Char 'x') (Char '/'))

createWorld :: Assets -> Stage' -> IOEvents World
createWorld assets (Stage' ws cs loc) = do
  sequence (spawnNow <$> ws)
  sequence (spawnNow <$> cs)
  spawnNow (pandaAtLocation assets loc)
  return $ World [] [] [] 0
