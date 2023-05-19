module Midriff.DQueue
  ( DQueue
  , dqNew
  , dqNewIO
  , dqWrite
  , dqIsEmpty
  , dqTryRead
  , dqRead
  , dqCap
  , dqFlush
  )
where

import Control.Concurrent.STM (STM)
import Midriff.Callback (Callback)
import Midriff.Ring (Cursor, Next, Ring, cursorFlush, cursorHasNext, cursorNew, cursorNewIO, cursorNext, cursorTryNext, ringCap, ringWrite)

-- | A /Dropping/ queue in 'STM'.
data DQueue a = DQueue
  { dqRing :: !(Ring a)
  , dqCursor :: !(Cursor a)
  }
  deriving stock (Eq)

-- | Create a new 'DQueue'. Capacity must be positive.
dqNew :: Ring a -> STM (DQueue a)
dqNew r = fmap (DQueue r) (cursorNew r)

dqNewIO :: Ring a -> IO (DQueue a)
dqNewIO r = fmap (DQueue r) (cursorNewIO r)

-- | Append an element to the 'DQueue'. Returns true if it overwrites an element.
dqWrite :: a -> DQueue a -> STM ()
dqWrite val = flip ringWrite val . dqRing

-- | Returns true if the 'DQueue' is empty.
dqIsEmpty :: DQueue a -> STM Bool
dqIsEmpty = fmap not . cursorHasNext . dqCursor

-- | Non-blocking read.
-- If the queue is non-empty, returns the first element and the number that have been dropped since last read.
-- If it is empty, returns Nothing.
dqTryRead :: DQueue a -> STM (Maybe (Next a))
dqTryRead = cursorTryNext . dqCursor

-- | Blocking read. Waits until an element is present in the queue and returns it
-- and the number that have been dropped sincel last read.
dqRead :: DQueue a -> STM (Next a)
dqRead = cursorNext . dqCursor

-- | Returns the capacity of the queue.
dqCap :: DQueue a -> Int
dqCap = ringCap . dqRing

-- | Flush at most capacity elements from the queue
dqFlush :: DQueue a -> Callback STM (Next a) -> IO ()
dqFlush = cursorFlush . dqCursor
