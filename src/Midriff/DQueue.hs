-- | A 'DQueue' is basically a ring buffer in STM, but it allows callers to be aware of overwrites.
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

data DQueueState a = DQueueState
  { dqsDropped :: !Int
  , dqsBody :: !(Seq a)
  } deriving (Eq, Show, Functor)

emptyDQueueState :: DQueueState a
emptyDQueueState = DQueueState 0 Seq.empty

-- TODO(ejconlon) Use STM TArray instead of a Seq?
data DQueue a = DQueue
  { dqCapacity :: !Int
  , dqState :: !(TVar (DQueueState a))
  }

newDQueue :: Int -> STM (DQueue a)
newDQueue cap =
  if cap <= 0
    then error "DQueue must have non-zero capacity"
    else fmap (DQueue cap) (TVar.newTVar emptyDQueueState)

-- Returns True if dropped events
writeDQueue :: a -> DQueue a -> STM Bool
writeDQueue val (DQueue cap mst) = TVar.stateTVar mst $ \(DQueueState dropped body) ->
  let (full, newDropped, newBody) =
        if Seq.length body == cap
          then (True, succ dropped, Seq.drop 1 body :|> val)
          else (False, dropped, body :|> val)
  in (full, DQueueState newDropped newBody)

isEmptyDQueue :: DQueue a -> STM Bool
isEmptyDQueue (DQueue _ mst) = fmap (\(DQueueState _ body) -> Seq.null body) (TVar.readTVar mst)

isFullDQueue :: DQueue a -> STM Bool
isFullDQueue (DQueue cap mst) = fmap (\(DQueueState _ body) -> Seq.length body == cap) (TVar.readTVar mst)

flushDQueue :: DQueue a -> STM (Int, [a])
flushDQueue (DQueue _ mst) = fmap (\(DQueueState dropped body) -> (dropped, toList body)) (TVar.swapTVar mst emptyDQueueState)

tryReadDQueue :: DQueue a -> STM (Maybe (Int, a))
tryReadDQueue (DQueue _ mst) = TVar.stateTVar mst $ \st@(DQueueState dropped body) ->
  case body of
    hd :<| newBody -> (Just (dropped, hd), DQueueState 0 newBody)
    _ -> (Nothing, st)

readDQueue :: DQueue a -> STM (Int, a)
readDQueue (DQueue _ mst) = TVar.readTVar mst >>= \(DQueueState dropped body) ->
  case body of
    hd :<| newBody -> do
      TVar.writeTVar mst (DQueueState 0 newBody)
      pure (dropped, hd)
    _ -> retry

lengthDQueue :: DQueue a -> STM Int
lengthDQueue (DQueue _ mst) = fmap (\(DQueueState _ body) -> Seq.length body) (TVar.readTVar mst)

capacityDQueue :: DQueue a -> Int
capacityDQueue = dqCapacity
