{-# LANGUAGE DeriveAnyClass #-}

module Midriff.CQueue
  ( CQueue
  , QueueEvent (..)
  , WriteResult (..)
  , newCQueue
  , newMaxCQueue
  , closeCQueue
  , isClosedCQueue
  , readCQueue
  , writeCQueue
  , sourceCQueue
  , sinkCQueue
  ) where

import Control.Applicative (liftA2)
import Control.Concurrent.STM (STM, atomically)
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Conduit (ConduitT, await, yield)
import GHC.Generics (Generic)
import Midriff.DQueue (DQueue, newDQueue, readDQueue, tryReadDQueue, writeDQueue)
import Midriff.TEvent (TEvent, isSetTEvent, newTEvent, setTEvent)

-- | A /Closable/ queue.
data CQueue a = CQueue
  { cqBody :: !(DQueue a)
  , cqEvent :: !TEvent
  } deriving stock (Eq, Generic)
    deriving anyclass (NFData)

newCQueue :: Int -> STM (CQueue a)
newCQueue cap = liftA2 CQueue (newDQueue cap) newTEvent

newMaxCQueue :: STM (CQueue a)
newMaxCQueue = newCQueue maxBound

closeCQueue :: CQueue a -> STM ()
closeCQueue (CQueue _ e) = setTEvent e

isClosedCQueue :: CQueue a -> STM Bool
isClosedCQueue (CQueue _ e) = isSetTEvent e

-- | Reads from the 'CQueue'.
--
-- If it's open, block until another element is enqueued.
-- If it's closed, return the next element, or 'Nothing'.
-- Once this returns 'Nothing', the queue is fully closed
-- and emptied, so it will never return anything else.
readCQueue :: CQueue a -> STM (Maybe (Int, a))
readCQueue (CQueue q e) = do
  -- If closed (for write), tryRead to drain, otherwise block read
  closed <- isSetTEvent e
  if closed
    then tryReadDQueue q
    else fmap Just (readDQueue q)

data WriteResult =
    ClosedResult
  | OkResult
  | DroppedResult
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

writeCQueue :: a -> CQueue a -> STM WriteResult
writeCQueue val (CQueue q e) = do
  -- If closed (for write), drop on floor
  closed <- isSetTEvent e
  if closed
    then pure ClosedResult
    else fmap (\b -> if b then DroppedResult else OkResult) (writeDQueue val q)

data QueueEvent a = QueueEvent
  { qeNum :: !Int
  , qeDropped :: !Int
  , qeVal :: !a
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

sourceCQueue :: MonadIO m => CQueue a -> ConduitT () (QueueEvent a) m ()
sourceCQueue cq = loop (0 :: Int) where
  loop !n = do
    mval <- liftIO (atomically (readCQueue cq))
    case mval of
      Just (d, v) -> do
        yield (QueueEvent n d v)
        loop (succ n)
      Nothing -> pure ()

sinkCQueue :: MonadIO m => CQueue a -> ConduitT a WriteResult m ()
sinkCQueue cq = loop where
  loop = do
    mval <- await
    case mval of
      Just val -> do
        res <- liftIO (atomically (writeCQueue val cq))
        yield res
        case res of
          ClosedResult -> pure ()
          _ -> loop
      Nothing -> pure ()
