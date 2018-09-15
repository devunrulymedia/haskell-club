{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Redux.TimerTest (htf_thisModulesTests) where

import Control.Lens
import Test.Framework

import Common.Redux
import Common.Timer
import Graphics.Gloss.Interface.IO.Game

data TestThing = TestThing
  { _toEnqueue :: Maybe (Float, String)
  , _timer :: Timer
  , _fired :: [String]
  }

makeLenses ''TestThing

updateTime :: Float -> TestThing -> Events TestThing
updateTime t w = case w ^. toEnqueue of
  Nothing     -> return w
  Just (d, e) -> do awaitEvent d e
                    return $ toEnqueue .~ Nothing $ w

reduceString :: String -> TestThing -> IOEvents TestThing
reduceString s w = return $ fired %~ (s :) $ w

ttRedux :: Redux TestThing
ttRedux = Redux
  { updater = updateTime
  , listener = noOp
  , reducer = focus reduceString
  }

testRedux :: Redux TestThing
testRedux = compose
  [ connect timerRedux timer
  , ttRedux
  ]

test_timer_works = do
  let initialTestThing = TestThing (Just (3, "event")) (Timer 0 []) []
  atTimeZero <- reduxUpdate testRedux 0 initialTestThing

  assertEqual [] (atTimeZero ^. fired)
  assertEqual 1 (length (atTimeZero ^. timer . pending))
  assertEqual Nothing (atTimeZero ^. toEnqueue)

  atTimeTwo <- reduxUpdate testRedux 2 atTimeZero

  assertEqual 1 (length (atTimeTwo ^. timer . pending))
  assertEqual [] (atTimeTwo ^. fired)

  atTimeFour <- reduxUpdate testRedux 2 atTimeTwo

  assertEqual 0 (length (atTimeFour ^. timer . pending))
  assertEqual ["event"] (atTimeFour ^. fired)
