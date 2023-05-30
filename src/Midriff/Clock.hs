module Midriff.Clock
  ( Clock
  , mkClock
  , clockPeriod
  , clockRead
  )
where

import Control.Concurrent (ThreadId, forkFinally, killThread)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO)
import Control.Monad (void, when)
import Data.Acquire (Acquire, mkAcquire)
import Midiot.Time (MonoTime, TimeDelta, currentTime, diffTime)
import Midriff.Control (Control, controlClose, controlIsOpen, controlNewIO)

data Clock = Clock
  { clockPeriod :: !TimeDelta
  , clockTid :: !ThreadId
  , clockTime :: !(TVar MonoTime)
  , clockControl :: !Control
  }

mkClockFrom :: MonoTime -> Control -> TimeDelta -> Acquire Clock
mkClockFrom start con per = mkAcquire alloc free
 where
  alloc = do
    var <- newTVarIO start
    tid <- forkFinally (update var con) (const (void (controlClose con)))
    pure (Clock per tid var con)
  update var con = do
    open <- atomically (controlIsOpen con)
    -- TODO bail early
    prev <- readTVarIO var
    now <- currentTime @MonoTime
    undefined
  free = killThread . clockTid

mkClock :: Control -> TimeDelta -> Acquire Clock
mkClock = mkClockFrom minBound

clockRead :: Clock -> STM MonoTime
clockRead = readTVar . clockTime
