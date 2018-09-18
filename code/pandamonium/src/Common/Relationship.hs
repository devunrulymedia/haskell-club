{-# LANGUAGE RankNTypes #-}

module Common.Relationship where

import Control.Lens
import Common.Redux

relationship :: (i -> a -> b -> Events (a, b))
             -> Lens c c a a
             -> Lens c c b b
             -> i -> c -> Events c
relationship f lensA lensB i c = do
  let a = c ^. lensA
  let b = c ^. lensB
  (a', b') <- f i a b
  return $ lensA .~ a' $ lensB .~ b' $ c

onList :: (i -> a -> b -> Events (a, b)) -> i -> a -> [b] -> Events (a, [b])
onList f i a [] = return (a, [])
onList f i a (b:bs) = do (a', b') <- f i a b
                         (a'', bs') <- onList f i a' bs
                         return (a'', b':bs')

onPairs :: (i -> a -> b -> Events (a, b)) -> i -> [a] -> [b] -> Events ([a], [b])
onPairs f i [] bs = return ([], bs)
onPairs f i (a:as) bs = do (a', bs') <- onList f i a bs
                           (as', bs'') <- onPairs f i as bs'
                           return (a':as', bs'')

againstSelf :: (i -> a -> a -> Events (a, a)) -> i -> [a] -> Events [a]
againstSelf f i [] = return []
againstSelf f i (a:as) = do (a', as') <- onList f i a as
                            as'' <- againstSelf f i as'
                            return (a':as'')
