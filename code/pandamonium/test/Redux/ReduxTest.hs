{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Redux.ReduxTest (htf_thisModulesTests) where

import Control.Lens
import Test.Framework

import Common.Redux2
import Graphics.Gloss.Interface.IO.Game

data LogicEvent = TimePassed Float | SystemEvent Event deriving (Eq, Show)

data TestThing = TestThing
  { _timePassed :: Float
  , _systemEvents :: [ Event ]
  , _logicEvents :: [ LogicEvent ]
  }

makeLenses ''TestThing

updateTime :: Float -> TestThing -> Events TestThing
updateTime t w = do
  fireEvent (TimePassed t)
  return $ timePassed %~ (+ t) $ w

listenEvent :: Event -> TestThing -> Events TestThing
listenEvent e w = do
  fireEvent (SystemEvent e)
  return $ systemEvents %~ (e :) $ w

reduceLogicEvent :: LogicEvent -> TestThing -> IOEvents TestThing
reduceLogicEvent e t = return $ logicEvents %~ (e :) $ t

testRedux :: Redux TestThing
testRedux = Redux
  { updater = updateTime
  , listener = listenEvent
  , reducer = concrify reduceLogicEvent
  }

test_update_via_redux = do
  let initialTestThing = TestThing 0 [] []
  updated <- reduxUpdate testRedux 3 initialTestThing
  assertEqual (updated ^. timePassed) 3

test_listen_via_redux = do
  let initialTestThing = TestThing 0 [] []
  updated <- reduxListen testRedux (EventMotion (0, 0)) initialTestThing
  assertEqual (updated ^. systemEvents) [ EventMotion (0, 0) ]

test_events_via_redux = do
  let initialTestThing = TestThing 0 [] []
  updated <- reduxUpdate testRedux 3 initialTestThing
  listened <- reduxListen testRedux (EventMotion (0, 0)) updated
  assertEqual (listened ^. logicEvents) [ SystemEvent (EventMotion (0, 0)), TimePassed 3 ]
