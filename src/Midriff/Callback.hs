module Midriff.Callback where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Data.Foldable (toList)
import Midriff.Gate (Gate (..))

type Callback m a = a -> m Gate

cbEmpty :: Applicative m => Callback m a
cbEmpty = const (pure GateOpen)

cbAndThen :: Monad m => Callback m a -> Callback m a -> Callback m a
cbAndThen h1 h2 a = do
  g <- h1 a
  case g of
    GateClosed -> pure GateClosed
    GateOpen -> h2 a

cbSerial :: (Monad m, Foldable f) => f (Callback m a) -> Callback m a
cbSerial cbs0 a = go (toList cbs0)
 where
  go = \case
    [] -> pure GateOpen
    cb : cbs -> do
      g <- cb a
      case g of
        GateClosed -> pure GateClosed
        GateOpen -> go cbs

cbContramap :: (a -> b) -> Callback m b -> Callback m a
cbContramap f h = h . f

cbContramapM :: Monad m => (a -> m b) -> Callback m b -> Callback m a
cbContramapM f h = f >=> h

cbContramapIO :: MonadIO m => (a -> IO b) -> Callback m b -> Callback m a
cbContramapIO f = cbContramapM (liftIO . f)

cbContramapU :: (MonadUnliftIO n, MonadIO m) => (a -> n b) -> Callback m b -> n (Callback m a)
cbContramapU f h = fmap (\r -> cbContramapIO (r . f) h) askRunInIO
