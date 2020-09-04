module Midriff.DQueue
  ( DQueue (..)
  , DQueueState
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
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

data DQueueState a = DQueueState
  { dqsBody :: !(Seq a)
  , dqsDropped :: !Int
  } deriving (Eq, Show, Functor)

emptyDQueueState :: DQueueState a
emptyDQueueState = DQueueState Seq.empty 0

data DQueue a = DQueue
  { dqCapacity :: !Int
  , dqState :: !(TVar (DQueueState a))
  }

newDQueue :: Int -> STM (DQueue a)
newDQueue cap =
  if cap <= 0
    then error "DQueue must have non-zero capacity"
    else fmap (DQueue cap) (TVar.newTVar emptyDQueueState)

writeDQueue :: a -> DQueue a -> STM Bool
writeDQueue val (DQueue cap mst) = TVar.stateTVar mst $ \(DQueueState body dropped) ->
  let (newBody, newDropped, full) =
        if Seq.length body == cap
          then (Seq.drop 1 body :|> val, succ dropped, True)
          else (body :|> val, dropped, False)
  in (full, DQueueState newBody newDropped)

isEmptyDQueue :: DQueue a -> STM Bool
isEmptyDQueue (DQueue _ mst) = fmap (\(DQueueState body _) -> Seq.null body) (TVar.readTVar mst)

isFullDQueue :: DQueue a -> STM Bool
isFullDQueue (DQueue cap mst) = fmap (\(DQueueState body _) -> Seq.length body == cap) (TVar.readTVar mst)

flushDQueue :: DQueue a -> STM (Int, Seq a)
flushDQueue (DQueue _ mst) = fmap (\(DQueueState body dropped) -> (dropped, body)) (TVar.swapTVar mst emptyDQueueState)

tryReadDQueue :: DQueue a -> STM (Maybe (Int, a))
tryReadDQueue (DQueue _ mst) = TVar.stateTVar mst $ \st@(DQueueState body dropped) ->
  case body of
    hd :<| newBody -> (Just (dropped, hd), DQueueState newBody 0)
    _ -> (Nothing, st)

readDQueue :: DQueue a -> STM (Int, a)
readDQueue (DQueue _ mst) = TVar.readTVar mst >>= \(DQueueState body dropped) ->
  case body of
    hd :<| newBody -> do
      TVar.writeTVar mst (DQueueState newBody 0)
      pure (dropped, hd)
    _ -> retry

lengthDQueue :: DQueue a -> STM Int
lengthDQueue (DQueue _ mst) = fmap (\(DQueueState body _) -> Seq.length body) (TVar.readTVar mst)

capacityDQueue :: DQueue a -> Int
capacityDQueue = dqCapacity
