{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Components.EntityTest (htf_thisModulesTests) where

import Test.Framework
import Common.Components.Entity

test_components_can_be_retrieved = do
  let ent = entity <-+ "Hello"
  let message = (from ent) :: Maybe String
  assertEqual message (Just "Hello")

test_nonexistent_components_can_be_safely_queried = do
  let ent = entity <-+ "Hello"
  let counter = (from ent) :: Maybe Int
  assertEqual counter Nothing

test_multiple_components_can_be_retrieved_regarldess_of_order = do
  let ent1 = entity <-+ "Hello" <-+ True
  assertEqual (from ent1) (Just "Hello")
  assertEqual (from ent1) (Just True)

  let ent2 = entity <-+ False <-+ "Goodbye"
  assertEqual (from ent2) (Just "Goodbye")
  assertEqual (from ent2) (Just False)
