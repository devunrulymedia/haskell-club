module Bomberman.Bomberman where

import Common.Redux
import Common.Components

bombermanRedux :: Redux World
bombermanRedux = compose [ physics, lifecycle ]
