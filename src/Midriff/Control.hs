module Midriff.Control
  ( Control (..)
  , controlIsOpen
  , controlAwait
  , controlNew
  , controlNewIO
  ) where

import Control.Monad ((>=>))
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, stateTVar)
import Midriff.Gate (Gate (..), gateIsOpen, gateAwait)

data Control = Control
  { controlGate :: !(STM Gate)
  -- ^ Read the gate status.
  , controlClose :: !(IO Bool)
  -- ^ Set the var. Returns true if this is the action that closes it.
  }

-- | Return true if the var has not been set, i.e. the gate is open.
-- Once it has been set, it remains so.
controlIsOpen :: Control -> STM Bool
controlIsOpen = fmap gateIsOpen . controlGate

-- | Await the var set, returning early if already set.
controlAwait :: Control -> STM ()
controlAwait = controlGate >=> gateAwait

newtype ControlVar = ControlVar {unControlVar :: TVar Gate}
  deriving stock (Eq)

cvNew :: STM ControlVar
cvNew = fmap ControlVar (newTVar GateOpen)

cvNewIO :: IO ControlVar
cvNewIO = fmap ControlVar (newTVarIO GateOpen)

cvGate :: ControlVar -> STM Gate
cvGate = readTVar . unControlVar

cvClose :: ControlVar -> IO Bool
cvClose = atomically . flip stateTVar (\g -> (g == GateOpen, GateClosed)) . unControlVar

cvControl :: ControlVar -> Control
cvControl cv = Control (cvGate cv) (cvClose cv)

controlNew :: STM Control
controlNew = fmap cvControl cvNew

controlNewIO :: IO Control
controlNewIO = fmap cvControl cvNewIO

