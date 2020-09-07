module Midriff.Connect
  ( InputState (..)
  , InputQueue
  , OutputState (..)
  , manageInputDevice
  , manageOutputDevice
  , manageInput
  , manageOutput
  , inputC
  , outputC
  , manageInputC
  , manageOutputC
  ) where

import Control.Concurrent.STM (atomically)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT, await)
import Data.Void (Void)
import Data.Word (Word8)
import Midriff.Config (PortConfig (..), DeviceConfig (..), Ignores (..), InputConfig (..), PortId (..))
import Midriff.CQueue (CQueue, closeCQueue, newCQueue, sourceCQueue, writeCQueue)
import Midriff.Resource (Manager, managedConduit, mkManager)
import Sound.RtMidi (InputDevice, OutputDevice, cancelCallback, closeDevice, closePort, createInput, createOutput,
                     defaultInput, defaultOutput, ignoreTypes, openPort, openVirtualPort, sendMessage, setCallback)

type InputCallback = Double -> [Word8] -> IO ()

type InputQueue = CQueue (Double, [Word8])

inputCb :: InputQueue -> InputCallback
inputCb cq fracSecs bytes = atomically (void (writeCQueue (fracSecs, bytes) cq))

data InputState = InputState
  { istDevice :: !InputDevice
  , istQueue :: !InputQueue
  }

acquireInputDevice :: Maybe DeviceConfig -> IO InputDevice
acquireInputDevice mpc = do
  case mpc of
    Nothing -> defaultInput
    -- Empty queue is fine for this, since we set a callback
    Just (DeviceConfig api client) -> createInput api client 0

manageInputDevice :: Maybe DeviceConfig -> Manager InputDevice
manageInputDevice dcfg = mkManager (acquireInputDevice dcfg) closeDevice

acquireOutputDevice :: Maybe DeviceConfig -> IO OutputDevice
acquireOutputDevice mpc = do
  case mpc of
    Nothing -> defaultOutput
    Just (DeviceConfig api client) -> createOutput api client

manageOutputDevice :: Maybe DeviceConfig -> Manager OutputDevice
manageOutputDevice dcfg = mkManager (acquireOutputDevice dcfg) closeDevice

acquireInput :: InputConfig -> InputDevice -> IO InputState
acquireInput (InputConfig (PortConfig name pid) cap migs) dev = do
  -- Create a close event so we can switch to non-blocking reads on close
  cq <- atomically (newCQueue cap)
  -- Set our ignored message types
  case migs of
    Nothing -> pure ()
    Just (Ignores x y z) -> ignoreTypes dev x y z
  -- Set our callback to enqueue to our own ring buffer
  setCallback dev (inputCb cq)
  -- Open the port for input
  case pid of
    PortIdReal pnum -> openPort dev pnum name
    PortIdVirtual -> openVirtualPort dev name
  pure (InputState dev cq)

releaseInput :: InputState -> IO ()
releaseInput (InputState dev cq) = do
  -- Switch to non-blocking reads first (in case the rest throws an exception)
  atomically (closeCQueue cq)
  -- Close the port for input
  closePort dev
  -- Remove our callback
  cancelCallback dev

manageInput :: InputConfig -> InputDevice -> Manager InputState
manageInput icfg dev = mkManager (acquireInput icfg dev) releaseInput

inputC :: MonadIO m => InputState -> ConduitT () (Int, (Double, [Word8])) m ()
inputC (InputState _ cq) = sourceCQueue cq

manageInputC :: MonadResource m => InputConfig -> InputDevice -> ConduitT () (Int, (Double, [Word8])) m ()
manageInputC icfg dev = managedConduit (manageInput icfg dev) inputC

newtype OutputState = OutputState
  { ostDevice :: OutputDevice
  }

acquireOutput :: PortConfig -> OutputDevice -> IO OutputState
acquireOutput (PortConfig name pid) dev = do
  -- Open the port for output
  case pid of
    PortIdReal pnum -> openPort dev pnum name
    PortIdVirtual -> openVirtualPort dev name
  pure (OutputState dev)

releaseOutput :: OutputState -> IO ()
releaseOutput (OutputState dev) = closePort dev

manageOutput :: PortConfig -> OutputDevice -> Manager OutputState
manageOutput cfg dev = mkManager (acquireOutput cfg dev) releaseOutput

outputC :: MonadIO m => OutputState -> ConduitT [Word8] Void m ()
outputC (OutputState dev) = loop where
  loop = do
    mbytes <- await
    case mbytes of
      Nothing -> pure ()
      Just bytes -> sendMessage dev bytes *> loop

manageOutputC :: MonadResource m => PortConfig -> OutputDevice -> ConduitT [Word8] Void m ()
manageOutputC cfg dev = managedConduit (manageOutput cfg dev) outputC
