module Midriff.CQueue
  ( CQueue
  , Written (..)
  , cqNew
  , cqNewIO
  , cqRead
  , cqTryRead
  , cqIsEmpty
  , cqWrite
  , cqFlush
  , cqCallback
  )
where

import Control.Concurrent.STM (STM, orElse)
import Midriff.Callback (Callback)
import Midriff.DQueue (DQueue, dqFlush, dqIsEmpty, dqNew, dqNewIO, dqRead, dqTryRead, dqWrite)
import Midriff.Gate (Gate (..))
import Midriff.Latch (Latch, RLatch (..), WLatch (..), latchAwait, latchIsOpen)
import Midriff.Ring (Next, Ring)

-- | A /Closable/ queue.
data CQueue a = CQueue
  { cqLatch :: !Latch
  , cqDropper :: !(DQueue a)
  }
  deriving stock (Eq)

instance RLatch (CQueue a) where
  latchGate = latchGate . cqLatch

instance WLatch (CQueue a) where
  latchClose = latchClose . cqLatch

cqNew :: Latch -> Ring a -> STM (CQueue a)
cqNew l = fmap (CQueue l) . dqNew

cqNewIO :: Latch -> Ring a -> IO (CQueue a)
cqNewIO l = fmap (CQueue l) . dqNewIO

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

cqCallback :: CQueue a -> Callback STM a
cqCallback cq a = flip fmap (cqWrite a cq) $
  \case WrittenYes -> GateOpen; WrittenNo -> GateClosed
