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

class MonadSuspend m o | m -> o where
    spend :: o -> m ()
    spend = adjust . const
    adjust :: (o -> o) -> m ()

instance (Monad m) => MonadSuspend (SuspendT o m) o where
  adjust f = SuspendT . lift $ modify f

newtype SuspendT o m a =
    SuspendT { runSuspend :: Coroutine (Request o o) (ReaderT (o -> Bool) (StateT o m)) a
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
      currentCost <- lift get
      costCondition <- lift ask
      when (costCondition currentCost) (request currentCost >>= lift . put)
      runSuspend $ f x'

suspendAtCost :: (Monad m) => o -> (o -> Bool) -> SuspendT o m a -> m (Either (SuspendT o m a) a)
suspendAtCost startCost condition (SuspendT crt) = flip evalStateT startCost . flip runReaderT condition $ do
    resume crt >>= \case
        Left (Request cst continue) -> return . Left . SuspendT $ continue cst
        Right a -> return $ Right a

test :: SuspendT (Sum Int) IO ()
test = do
    liftIO $ print "one"
    spend 20
    liftIO $ print "two"
    spend 30
    liftIO $ print "three"
    spend 40
    liftIO $ print "four"



