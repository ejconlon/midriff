module Midriff.Unlifted
  ( DQueue
  , newDQueueUnlifted
  , newMaxDQueueUnlifted
  , writeDQueueUnlifted
  ) where

import Control.Concurrent.STM (atomically)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Conduit (ConduitT, await, yield)
import Data.Void (Void)
import Midriff.DQueue (DQueue, newDQueue, readDQueue, writeDQueue)

newDQueueUnlifted :: MonadIO m => Int -> m (DQueue a)
newDQueueUnlifted = liftIO . atomically . newDQueue

newMaxDQueueUnlifted :: MonadIO m => m (DQueue a)
newMaxDQueueUnlifted = newDQueueUnlifted maxBound

writeDQueueUnlifted :: MonadIO m => a -> DQueue a -> m Bool
writeDQueueUnlifted val = liftIO . atomically . writeDQueue val

sourceDQueue :: MonadIO m => DQueue a -> ConduitT Void (Int, a) m Void
sourceDQueue q = forever (liftIO (atomically (readDQueue q)) >>= yield)

sinkDQueue :: MonadIO m => DQueue a -> ConduitT a Bool m ()
sinkDQueue q = forever $ do
  mval <- await
  case mval of
    Just val -> writeDQueueUnlifted val q >>= yield
    Nothing -> pure ()
