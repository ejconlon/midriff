module Midriff.Latch
  ( Latch
  , latchNew
  , latchNewIO
  , latchIsOpen
  , latchGate
  , latchClose
  , latchAwait
  )
where

import Control.Monad (void)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, newEmptyTMVarIO, tryReadTMVar, readTMVar, tryPutTMVar)
import Data.Maybe (isNothing)
import Midriff.Gate (Gate (..))

-- | An variable that starts unset (GateOpen) and can be set once (GateClosed).
-- This is useful for recording one-way transitions.
newtype Latch = Latch {unLatch :: TMVar ()}
  deriving stock (Eq)

-- | Create a new 'Latch'.
latchNew :: STM Latch
latchNew = fmap Latch newEmptyTMVar

-- | Create a new 'Latch'.
latchNewIO :: IO Latch
latchNewIO = fmap Latch newEmptyTMVarIO

-- | Read the gate status of the latch.
latchGate :: Latch -> STM Gate
latchGate = fmap (maybe GateOpen (const GateClosed)) .  tryReadTMVar . unLatch

-- | Return true if the var has not been set, i.e. the gate is open.
-- Once it has been set, it remains so.
latchIsOpen :: Latch -> STM Bool
latchIsOpen = fmap isNothing . tryReadTMVar . unLatch

-- | Set the var. Idempotent; additional calls have no observable effect.
latchClose :: Latch -> STM ()
latchClose = void . flip tryPutTMVar () . unLatch

-- | Await the var set, returning early if already set.
latchAwait :: Latch -> STM ()
latchAwait = readTMVar . unLatch
