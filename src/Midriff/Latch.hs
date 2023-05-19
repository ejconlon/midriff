module Midriff.Latch
  ( RLatch (..)
  , latchIsOpen
  , latchAwait
  , WLatch (..)
  , latchCloseWith
  , Latch
  , latchNew
  , latchNewIO
  )
where

import Control.Concurrent.STM (STM, atomically, check)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, writeTVar)
import Control.Monad (when, (>=>))
import Midriff.Gate (Gate (..))

class RLatch a where
  -- | Read the gate status.
  latchGate :: a -> STM Gate

-- | Return true if the var has not been set, i.e. the gate is open.
-- Once it has been set, it remains so.
latchIsOpen :: RLatch a => a -> STM Bool
latchIsOpen = fmap (== GateOpen) . latchGate

-- | Await the var set, returning early if already set.
latchAwait :: RLatch a => a -> STM ()
latchAwait = latchGate >=> check . (== GateClosed)

class RLatch a => WLatch a where
  -- | Set the var. Idempotent; additional calls have no observable effect.
  latchClose :: a -> STM ()

-- | Run the clean up action on close (if currently open)
latchCloseWith :: WLatch a => a -> IO b -> IO (Maybe b)
latchCloseWith a act = do
  cleanup <- atomically $ do
    open <- latchIsOpen a
    when open (latchClose a)
    pure open
  if cleanup then fmap Just act else pure Nothing

-- | An variable that starts unset (GateOpen) and can be set once (GateClosed).
-- This is useful for recording one-way transitions.
newtype Latch = Latch {unLatch :: TVar Gate}
  deriving stock (Eq)

instance RLatch Latch where
  latchGate = readTVar . unLatch

instance WLatch Latch where
  latchClose = flip writeTVar GateClosed . unLatch

-- | Create a new 'Latch'.
latchNew :: STM Latch
latchNew = fmap Latch (newTVar GateOpen)

-- | Create a new 'Latch'.
latchNewIO :: IO Latch
latchNewIO = fmap Latch (newTVarIO GateOpen)
