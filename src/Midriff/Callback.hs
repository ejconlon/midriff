module Midriff.Callback where

import Midriff.Gate (Gate (..))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Data.Foldable (toList)
import Data.Functor.Contravariant (Contravariant (..))

newtype Callback m a = Callback { cbRun :: a -> m Gate }

instance Contravariant (Callback m) where
  contramap f (Callback h) = Callback (h . f)

cbEmpty :: Applicative m => Callback m a
cbEmpty = Callback (const (pure GateOpen))

cbAndThen :: Monad m => Callback m a -> Callback m a -> Callback m a
cbAndThen (Callback h1) (Callback h2) = Callback $ \a -> do
  g <- h1 a
  case g of
    GateClosed -> pure GateClosed
    GateOpen -> h2 a

cbSerial :: (Monad m, Foldable f) => f (Callback m a) -> Callback m a
cbSerial cbs0 = Callback (\a -> go a (toList cbs0)) where
  go a = \case
    [] -> pure GateOpen
    cb:cbs -> do
      g <- cbRun cb a
      case g of
        GateClosed -> pure GateClosed
        GateOpen -> go a cbs

cbContramapU :: (MonadUnliftIO n, MonadIO m) => (a -> n b) -> Callback m b -> n (Callback m a)
cbContramapU f (Callback h) = fmap (\r -> Callback (liftIO . r . f >=> h)) askRunInIO

cbContramapIO :: MonadIO m => (a -> IO b) -> Callback m b -> Callback m a
cbContramapIO f = cbContramapM (liftIO . f)

cbContramapM :: Monad m => (a -> m b) -> Callback m b -> Callback m a
cbContramapM f (Callback h) = Callback (f >=> h)

