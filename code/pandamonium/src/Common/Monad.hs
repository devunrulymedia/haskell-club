module Common.Monad where

import Data.Maybe

purely :: Monad m => (a -> b) -> a -> m b
purely f a = return $ f a

purely2 :: Monad m => (a -> b -> c) -> a -> b -> m c
purely2 f a b = return $ f a b

ifMaybe :: Bool -> Maybe a -> Maybe a
ifMaybe True a  = a
ifMaybe False _ = Nothing

whenJust :: Monad m => Maybe (m ()) -> m ()
whenJust = fromMaybe (return ())
