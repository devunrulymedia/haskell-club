{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Components.EntityTest (htf_thisModulesTests) where

import Test.Framework
import Common.Components.Entity

instance Component [ Char ]
instance Component Bool
instance Component Int

test_components_can_be_retrieved = do
  let ent = entity 0 <-+ "Hello"
  let message = (extract ent) :: Maybe String
  assertEqual message (Just "Hello")

test_nonexistent_components_can_be_safely_queried = do
  let ent = entity 0 <-+ "Hello"
  let counter = (extract ent) :: Maybe Int
  assertEqual counter Nothing

test_multiple_components_can_be_retrieved_regarldess_of_order = do
  let ent1 = entity 0 <-+ "Hello" <-+ True
  assertEqual (extract ent1) (Just "Hello")
  assertEqual (extract ent1) (Just True)

  let ent2 = entity 0 <-+ False <-+ "Goodbye"
  assertEqual (extract ent2) (Just "Goodbye")
  assertEqual (extract ent2) (Just False)
