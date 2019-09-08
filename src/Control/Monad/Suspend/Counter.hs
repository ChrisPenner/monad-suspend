{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Suspend.Counter where

import Control.Monad.Suspend
import Control.Monad
import Control.Monad.IO.Class

newtype CounterT o m a = CounterT {runCounterT :: SuspendT o m a}
    deriving newtype Functor
    deriving newtype MonadIO

instance (Functor m, Monad m, Enum o) => Applicative (CounterT o m) where
  pure = return
  (<*>) = ap

instance (Enum o, Monad m) => Monad (CounterT o m) where
  return = CounterT . return
  CounterT s >>= f = CounterT $ do
    adjust succ
    s >>= runCounterT . f

counter :: CounterT Int IO Int
counter = do
    x <- return 1
    liftIO $ print x
    y <- return 2
    liftIO $ print y
    return (x + y)
