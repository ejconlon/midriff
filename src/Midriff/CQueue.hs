module Midriff.CQueue
  ( CQueue
  , Written (..)
  , cqNewSimple
  , cqNew
  , cqNewIO
  , cqLatch
  , cqRead
  , cqTryRead
  , cqIsEmpty
  , cqWrite
  , cqFlush
  , cqSource
  , cqSink
  )
where

import Midriff.Callback (Callback)
import Midriff.Ring (Ring, Next)
import Control.Applicative (liftA2)
import Control.Concurrent.STM (STM, atomically, orElse)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Conduit (ConduitT, await, yield)
import Midriff.DQueue (DQueue, dqNewSimple, dqNew, dqNewIO, dqRead, dqTryRead, dqWrite, dqFlush, dqIsEmpty)
import Midriff.Latch (Latch, latchNew, latchNewIO, latchAwait, latchIsOpen)

-- | A /Closable/ queue.
data CQueue a = CQueue
  { cqLatch :: !Latch
  , cqDropper :: !(DQueue a)
  } deriving stock (Eq)

cqNewSimple :: Int -> IO (CQueue a)
cqNewSimple = liftA2 CQueue latchNewIO . dqNewSimple

cqNew :: Ring a -> STM (CQueue a)
cqNew = liftA2 CQueue latchNew . dqNew

cqNewIO :: Ring a -> IO (CQueue a)
cqNewIO = liftA2 CQueue latchNewIO . dqNewIO

-- | Reads from the 'CQueue'.
--
-- If it's open, block until another element is enqueued.
-- If it's closed, return the next element, or 'Nothing'.
-- Once this returns 'Nothing', the queue is fully closed
-- and emptied, so it will never return anything else.
cqRead :: CQueue a -> STM (Maybe (Next a))
cqRead (CQueue l d) = orElse (fmap Just (dqRead d)) (Nothing <$ latchAwait l)

cqTryRead :: CQueue a -> STM (Maybe (Next a))
cqTryRead = dqTryRead . cqDropper

cqIsEmpty :: CQueue a -> STM Bool
cqIsEmpty = dqIsEmpty . cqDropper

data Written
  = WrittenYes
  | WrittenNo
  deriving stock (Eq, Show, Enum, Bounded)

cqWrite :: a -> CQueue a -> STM Written
cqWrite val (CQueue l d) = do
  open <- latchIsOpen l
  if open
    then WrittenYes <$ dqWrite val d
    else pure WrittenNo

-- | Flush at most capacity elements from the queue
cqFlush :: CQueue a -> Callback STM (Next a) -> IO ()
cqFlush = dqFlush . cqDropper

cqSource :: MonadIO m => CQueue a -> ConduitT () (Next a) m ()
cqSource cq = go
 where
  go = do
    mn <- liftIO (atomically (cqRead cq))
    case mn of
      Just n -> yield n *> go
      Nothing -> pure ()

cqSink :: MonadIO m => CQueue a -> ConduitT a () m Written
cqSink cq = go
 where
  go = do
    mval <- await
    case mval of
      Just val -> do
        res <- liftIO (atomically (cqWrite val cq))
        case res of
          WrittenNo -> pure WrittenNo
          _ -> go
      Nothing -> pure WrittenYes

