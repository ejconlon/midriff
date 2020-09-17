module Midriff.DQueue
  ( DQueue
  , newDQueue
  , writeDQueue
  , isEmptyDQueue
  , isFullDQueue
  , flushDQueue
  , tryReadDQueue
  , readDQueue
  , lengthDQueue
  , capacityDQueue
  ) where

import Control.Concurrent.STM (STM, retry)
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

-- TODO(ejconlon) Use STM TArray instead of a Seq?
data DQueueState a = DQueueState
  { dqsDropped :: !Int
  , dqsBody :: !(Seq a)
  }

emptyDQueueState :: DQueueState a
emptyDQueueState = DQueueState 0 Seq.empty

-- | A /Dropping/ queue in 'STM'. Otherwise known as a ring-buffer.
-- The methods exposed will let you know if elements have been dropped
-- since the last operation.
data DQueue a = DQueue
  { dqCapacity :: !Int
  , dqState :: !(TVar (DQueueState a))
  }

-- | Create a new 'DQueue'. Capacity must be positive.
newDQueue :: Int -> STM (DQueue a)
newDQueue cap =
  if cap <= 0
    then error "DQueue must have non-zero capacity"
    else fmap (DQueue cap) (TVar.newTVar emptyDQueueState)

-- | Append an element to the 'DQueue'. Returns true if it overwrites an element.
writeDQueue :: a -> DQueue a -> STM Bool
writeDQueue val (DQueue cap mst) = TVar.stateTVar mst $ \(DQueueState dropped body) ->
  let (full, newDropped, newBody) =
        if Seq.length body == cap
          then (True, succ dropped, Seq.drop 1 body :|> val)
          else (False, dropped, body :|> val)
  in (full, DQueueState newDropped newBody)

-- | Returns true if the 'DQueue' is empty.
isEmptyDQueue :: DQueue a -> STM Bool
isEmptyDQueue (DQueue _ mst) = fmap (\(DQueueState _ body) -> Seq.null body) (TVar.readTVar mst)

-- | Returns true if the 'DQueue' is full.
isFullDQueue :: DQueue a -> STM Bool
isFullDQueue (DQueue cap mst) = fmap (\(DQueueState _ body) -> Seq.length body == cap) (TVar.readTVar mst)

-- | Returns all elements and the number that have been dropped since last read. Clears the queue.
flushDQueue :: DQueue a -> STM (Int, [a])
flushDQueue (DQueue _ mst) = fmap (\(DQueueState dropped body) -> (dropped, toList body)) (TVar.swapTVar mst emptyDQueueState)

-- | Non-blocking read.
-- If the queue is non-empty, returns the first element and the number that have been dropped since last read.
-- If it is empty, returns Nothing.
tryReadDQueue :: DQueue a -> STM (Maybe (Int, a))
tryReadDQueue (DQueue _ mst) = TVar.stateTVar mst $ \st@(DQueueState dropped body) ->
  case body of
    hd :<| newBody -> (Just (dropped, hd), DQueueState 0 newBody)
    _ -> (Nothing, st)

-- | Blocking read. Waits until an element is present in the queue and returns it
-- and the number that have been dropped sincel last read.
readDQueue :: DQueue a -> STM (Int, a)
readDQueue (DQueue _ mst) = TVar.readTVar mst >>= \(DQueueState dropped body) ->
  case body of
    hd :<| newBody -> do
      TVar.writeTVar mst (DQueueState 0 newBody)
      pure (dropped, hd)
    _ -> retry

-- | Returns the length of the queue, obeying 0 <= length <= capacity.
lengthDQueue :: DQueue a -> STM Int
lengthDQueue (DQueue _ mst) = fmap (\(DQueueState _ body) -> Seq.length body) (TVar.readTVar mst)

-- | Returns the capacity of the queue.
capacityDQueue :: DQueue a -> Int
capacityDQueue = dqCapacity
