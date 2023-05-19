module Midriff.Clock
  ( Clock
  , acquireClock
  , acquireClockFork
  , clockPeriod
  , clockRead
  -- , clockIsOpen
  -- , clockGate
  -- , clockAwait
  )
where

import Control.Concurrent (ThreadId, forkFinally, killThread)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO)
import Data.Acquire (Acquire)
import Midriff.Latch (Latch, latchClose, latchNewIO)
import Midriff.Time (MonoTime, TimeDelta)

data Clock = Clock
  { clockPeriod :: !TimeDelta
  , clockTid :: !ThreadId
  , clockLatch :: !Latch
  , clockVar :: !(TVar MonoTime)
  }

acquireClockFrom :: MonoTime -> Latch -> TimeDelta -> Acquire Clock
acquireClockFrom latch start per = undefined -- manageNew alloc free where
-- alloc = do
--   v <- newTVarIO start
--   l <- latchNewIO
--   tid <- forkFinally (atomically (latchClose l)) (loop l v)
--   pure (Clock per tid l v)
-- loop _l _v = go where
--   go = error "TODO"
-- free = killThread . clockTid

acquireClock :: Latch -> TimeDelta -> Acquire Clock
acquireClock = acquireClockFrom minBound

acquireClockFork :: Rational -> Clock -> Acquire Clock
acquireClockFork = error "TODO"

clockRead :: Clock -> STM MonoTime
clockRead = readTVar . clockVar
