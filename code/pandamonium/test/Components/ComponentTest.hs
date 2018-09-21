{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Components.ComponentTest (htf_thisModulesTests) where

import Test.Framework
import Common.Components

test_components_can_be_retrieved = do
  let entity = components <-+ "Hello"
  let message = (from entity) :: Maybe String
  assertEqual message (Just "Hello")

test_nonexistent_components_can_be_safely_queried = do
  let entity = components <-+ "Hello"
  let counter = (from entity) :: Maybe Int
  assertEqual counter Nothing

test_multiple_components_can_be_retrieved_regarldess_of_order = do
  let entity1 = components <-+ "Hello" <-+ True
  assertEqual (from entity1) (Just "Hello")
  assertEqual (from entity1) (Just True)

  let entity2 = components <-+ False <-+ "Goodbye"
  assertEqual (from entity2) (Just "Goodbye")
  assertEqual (from entity2) (Just False)
