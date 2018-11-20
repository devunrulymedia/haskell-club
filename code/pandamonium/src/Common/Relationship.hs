{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module Common.Relationship where

import Control.Lens

type Update a b = Lens a a b b

relationshipM :: Monad m
             => (a -> b -> m (a, b))
             -> Update c a
             -> Update c b
             -> c -> m c
relationshipM f lensA lensB c = do
  let a = c ^. lensA
  let b = c ^. lensB
  (a', b') <- f a b
  return $ lensA .~ a' $ lensB .~ b' $ c

relationshipMWith :: Monad m
             => (i -> a -> b -> m (a, b))
             -> Update c a
             -> Update c b
             -> i -> c -> m c
relationshipMWith f lensA lensB i = relationshipM (f i) lensA lensB

relationship :: (a -> b -> (a, b))
             -> Update c a
             -> Update c b
             -> c -> c
relationship f lensA lensB c = let (a, b) = f (c ^. lensA) (c ^. lensB)
                                in lensA .~ a $ lensB .~ b $ c

relationshipWith :: (i -> a -> b -> (a, b))
                 -> Lens c c a a
                 -> Lens c c b b
                 -> i -> c -> c
relationshipWith f lensA lensB i c = relationship (f i) lensA lensB c

onList :: Monad m => (a -> b -> m (a, b)) -> a -> [b] -> m (a, [b])
onList f a [] = return (a, [])
onList f a (b:bs) = do (a', b') <- f a b
                       (a'', bs') <- onList f a' bs
                       return (a'', b':bs')

onPairs :: Monad m => (a -> b -> m (a, b)) -> [a] -> [b] -> m ([a], [b])
onPairs f [] bs = return ([], bs)
onPairs f (a:as) bs = do (a', bs') <- onList f a bs
                         (as', bs'') <- onPairs f as bs'
                         return (a':as', bs'')

againstSelf :: Monad m => (a -> a -> m (a, a)) -> [a] -> m [a]
againstSelf f [] = return []
againstSelf f (a:as) = do (a', as') <- onList f a as
                          as'' <- againstSelf f as'
                          return (a':as'')
