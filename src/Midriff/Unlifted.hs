module Midriff.Unlifted
  ( DQueue
  , newDQueueUnlifted
  , writeDQueueUnlifted
  , eventReadDQueueUnlifted
  , eventWriteDQueueUnlifted
  , sourceDQueue
  , sinkDQueue
  ) where

import Control.Concurrent.STM (atomically)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Conduit (ConduitT, await, yield)
import Data.Void (Void)
import Midriff.DQueue (DQueue, newDQueue, readDQueue, tryReadDQueue, writeDQueue)
import Midriff.TEvent (TEvent, isSetTEvent)

newDQueueUnlifted :: MonadIO m => Int -> m (DQueue a)
newDQueueUnlifted = liftIO . atomically . newDQueue

writeDQueueUnlifted :: MonadIO m => a -> DQueue a -> m Bool
writeDQueueUnlifted val = liftIO . atomically . writeDQueue val

eventReadDQueueUnlifted :: MonadIO m => DQueue a -> TEvent -> m (Maybe (Int, a))
eventReadDQueueUnlifted q e = liftIO $ atomically $ do
  -- If closed (for write), tryRead to drain, otherwise block read
  closed <- isSetTEvent e
  if closed
    then tryReadDQueue q
    else fmap Just (readDQueue q)

eventWriteDQueueUnlifted :: MonadIO m => DQueue a -> TEvent -> a -> m Bool
eventWriteDQueueUnlifted q e val = liftIO $ atomically $ do
  -- If closed (for write), drop on floor
  -- Return true if "data was lost" due to closed or full
  closed <- isSetTEvent e
  if closed
    then pure True
    else writeDQueue val q

sourceDQueue :: MonadIO m => DQueue a -> TEvent -> ConduitT Void (Int, a) m ()
sourceDQueue q e = forever $ do
  mval <- eventReadDQueueUnlifted q e
  case mval of
    Just val -> yield val
    Nothing -> pure ()

sinkDQueue :: MonadIO m => DQueue a -> ConduitT a Bool m ()
sinkDQueue q = forever $ do
  mval <- await
  case mval of
    Just val -> writeDQueueUnlifted val q >>= yield
    Nothing -> pure ()
