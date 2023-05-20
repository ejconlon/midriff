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
import Midriff.Ring (Next, Ring)
import Midriff.Control (Control, controlAwait, controlIsOpen)

-- | A /Closable/ queue.
data CQueue a = CQueue
  { cqControl :: !Control
  , cqDropper :: !(DQueue a)
  }

cqNew :: Control -> Ring a -> STM (CQueue a)
cqNew c = fmap (CQueue c) . dqNew

cqNewIO :: Control -> Ring a -> IO (CQueue a)
cqNewIO c = fmap (CQueue c) . dqNewIO

-- | Reads from the 'CQueue'.
--
-- If it's open, block until another element is enqueued.
-- If it's closed, return the next element, or 'Nothing'.
-- Once this returns 'Nothing', the queue is fully closed
-- and emptied, so it will never return anything else.
cqRead :: CQueue a -> STM (Maybe (Next a))
cqRead (CQueue c d) = orElse (fmap Just (dqRead d)) (Nothing <$ controlAwait c)

cqTryRead :: CQueue a -> STM (Maybe (Next a))
cqTryRead = dqTryRead . cqDropper

cqIsEmpty :: CQueue a -> STM Bool
cqIsEmpty = dqIsEmpty . cqDropper

data Written
  = WrittenYes
  | WrittenNo
  deriving stock (Eq, Show, Enum, Bounded)

cqWrite :: a -> CQueue a -> STM Written
cqWrite val (CQueue c d) = do
  open <- controlIsOpen c
  if open
    then WrittenYes <$ dqWrite val d
    else pure WrittenNo

-- | Flush at most capacity elements from the queue
cqFlush :: CQueue a -> Callback STM (Next a) -> IO ()
cqFlush = dqFlush . cqDropper

cqCallback :: CQueue a -> Callback STM a
cqCallback cq a = flip fmap (cqWrite a cq) $
  \case WrittenYes -> GateOpen; WrittenNo -> GateClosed
