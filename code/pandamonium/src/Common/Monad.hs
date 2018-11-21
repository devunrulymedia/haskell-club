module Common.Monad where

purely :: Monad m => (a -> b) -> a -> m b
purely f a = return $ f a

purely2 :: Monad m => (a -> b -> c) -> a -> b -> m c
purely2 f a b = return $ f a b
