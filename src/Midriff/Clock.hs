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
import Midriff.Time (MonoTime, TimeDelta)
import Midriff.Control (Control)

data Clock = Clock
  { clockPeriod :: !TimeDelta
  , clockTid :: !ThreadId
  , clockControl :: !Control
  , clockVar :: !(TVar MonoTime)
  }

acquireClockFrom :: MonoTime -> Control -> TimeDelta -> Acquire Clock
acquireClockFrom control start per = undefined -- manageNew alloc free where
-- alloc = do
--   v <- newTVarIO start
--   l <- latchNewIO
--   tid <- forkFinally (atomically (latchClose l)) (loop l v)
--   pure (Clock per tid l v)
-- loop _l _v = go where
--   go = error "TODO"
-- free = killThread . clockTid

acquireClock :: Control -> TimeDelta -> Acquire Clock
acquireClock = acquireClockFrom minBound

acquireClockFork :: Rational -> Clock -> Acquire Clock
acquireClockFork = error "TODO"

clockRead :: Clock -> STM MonoTime
clockRead = readTVar . clockVar
