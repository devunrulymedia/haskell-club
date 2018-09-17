{-# LANGUAGE RankNTypes #-}

module Common.Relationship where

import Control.Lens
import Common.Redux

relationship :: (a -> b -> Events (a, b))
             -> Lens c c a a
             -> Lens c c b b
             -> i -> c -> Events c
relationship f lensA lensB i c = do
  let a = c ^. lensA
  let b = c ^. lensB
  (a', b') <- f a b
  return $ lensA .~ a' $ lensB .~ b' $ c

onList :: (a -> b -> Events (a, b)) -> a -> [b] -> Events (a, [b])
onList f a [] = return (a, [])
onList f a (b:bs) = do (a', b') <- f a b
                       (a'', bs') <- onList f a' bs
                       return (a'', b':bs')

onPairs :: (a -> b -> Events (a, b)) -> [a] -> [b] -> Events ([a], [b])
onPairs f [] bs = return ([], bs)
onPairs f (a:as) bs = do (a', bs') <- onList f a bs
                         (as', bs'') <- onPairs f as bs'
                         return (a':as', bs'')
