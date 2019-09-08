{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Suspend where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import Data.Functor
import Data.Bifunctor

class MonadSuspend m o | m -> o where
    spend :: o -> m ()
    spend = adjust . const
    adjust :: (o -> o) -> m ()

instance (Monad m) => MonadSuspend (SuspendT o m) o where
  adjust f = SuspendT . lift $ modify f

newtype SuspendT o m a =
    SuspendT { runSuspend :: Coroutine (Yield ()) (ReaderT (o -> Bool) (StateT o m)) a
             }
    deriving stock Functor
    deriving newtype MonadIO

instance (Monad m) => Applicative (SuspendT o m) where
  pure = SuspendT . pure
  (<*>) = ap

instance (Monad m) => Monad (SuspendT o m) where
  return = SuspendT . return
  SuspendT x >>= f = SuspendT $ do
      x' <- x
      lift ask <*> lift get
      currentCost <- lift get
      costCondition <- lift ask
      when (costCondition currentCost) (yield ())
      runSuspend $ f x'

suspendAtCost :: (Monad m)
              => o
              -> (o -> Bool)
              -> SuspendT o m r
              -> m (Either (SuspendT o m r) r, o)
suspendAtCost startCost condition (SuspendT coroutine) =
    flip runStateT startCost . flip runReaderT condition $ do
        resume coroutine <&> \case
            Left (Yield () continue) -> Left . SuspendT $ continue
            Right a -> Right a

test :: SuspendT (Sum Int) IO ()
test = do
    liftIO $ print "one"
    spend 20
    liftIO $ print "two"
    spend 30
    liftIO $ print "three"
    spend 40
    liftIO $ print "four"
