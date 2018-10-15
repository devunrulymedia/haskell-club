{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Framerate (withFps, fpsRedux) where

import Control.Lens
import Control.Monad.Trans

import Common.Redux
import Common.Renderable

data Fps = Fps Int deriving ReduxEvent

data FramerateCounter g = FramerateCounter
  { _fps :: Int
  , _soFar :: Int
  , _inSecond :: Float
  , _game :: g
  }

makeLenses ''FramerateCounter

withFps :: g -> FramerateCounter g
withFps g = FramerateCounter { _fps = 0, _soFar = 0, _inSecond = 0, _game = g }

instance (Renderable g) => Renderable (FramerateCounter g) where
  render f = render (f ^. game)

updateFramerate :: Float -> FramerateCounter g -> Events (FramerateCounter g)
updateFramerate t f = let f' = (soFar +~ 1) $ (inSecond +~ t) $ f
                       in if (f' ^. inSecond > 1)
                          then do let thisFps = f' ^. soFar
                                  fireEvent (Fps thisFps)
                                  return $ soFar .~ 0 $ fps .~ thisFps $ inSecond -~ 1 $ f'
                          else return f'

-- temporary, until i get the renderer up and running
reduceFramerate :: Fps -> FramerateCounter g -> IOEvents (FramerateCounter g)
reduceFramerate (Fps f) w = do liftIO (putStrLn $ "Fps: " ++ show f)
                               return w

fpsRedux' :: Redux (FramerateCounter g)
fpsRedux' = Redux { updater = updateFramerate, listener = noOp, reducer = focusM reduceFramerate }

fpsRedux :: Redux g -> Redux (FramerateCounter g)
fpsRedux r = compose [ connect r game, fpsRedux' ]
