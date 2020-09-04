module Midriff.Conn
  ( ConnResult
  , withInputDevice
  , withOutputDevice
  , inputConn
  , outputConn
  ) where

import Control.Concurrent.STM (atomically)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT, await, bracketP)
import Data.Void (Void)
import Data.Word (Word8)
import Midriff.Config (Config (..), DeviceConfig (..), Ignores (..), InputConfig (..), PortId (..))
import Midriff.TEvent (TEvent, newTEvent, setTEvent)
import Midriff.Unlifted (DQueue, eventWriteDQueueUnlifted, newDQueueUnlifted, sourceDQueue)
import Sound.RtMidi (Error, InputDevice, OutputDevice, cancelCallback, closeDevice, closePort, createInput,
                     createOutput, defaultInput, defaultOutput, ignoreTypes, openPort, openVirtualPort, sendMessage,
                     setCallback)
import UnliftIO.Exception (bracket)

type InputCallback = Double -> [Word8] -> IO ()

type InputQueue = DQueue (Double, [Word8])

data InputState = InputState
  { istDevice :: !InputDevice
  , istCloseEvent :: !TEvent
  }

inputDeviceAcquire :: Maybe DeviceConfig -> IO InputDevice
inputDeviceAcquire mpc = do
  case mpc of
    Nothing -> defaultInput
    -- Empty queue is fine for this, since we set a callback
    Just (DeviceConfig api client) -> createInput api client 0

inputDeviceRelease :: InputDevice -> IO ()
inputDeviceRelease = closeDevice

withInputDevice :: MonadUnliftIO m => Maybe DeviceConfig -> (InputDevice -> m a) -> m a
withInputDevice dcfg = bracket (liftIO (inputDeviceAcquire dcfg)) (liftIO . inputDeviceRelease)

outputDeviceAcquire :: Maybe DeviceConfig -> IO OutputDevice
outputDeviceAcquire mpc = do
  case mpc of
    Nothing -> defaultOutput
    Just (DeviceConfig api client) -> createOutput api client

outputDeviceRelease :: OutputDevice -> IO ()
outputDeviceRelease = closeDevice

withOutputDevice :: MonadUnliftIO m => Maybe DeviceConfig -> (OutputDevice -> m a) -> m a
withOutputDevice dcfg = bracket (liftIO (outputDeviceAcquire dcfg)) (liftIO . outputDeviceRelease)

inputAcquire :: InputConfig -> (TEvent -> InputCallback) -> InputDevice -> IO InputState
inputAcquire (InputConfig (Config name pid) migs) cb d = do
  -- Create a close event so we can switch to non-blocking reads on close
  c <- atomically newTEvent
  -- Set our ignored message types
  case migs of
    Nothing -> pure ()
    Just (Ignores x y z) -> ignoreTypes d x y z
  -- Set our callback to enqueue to our own ring buffer
  setCallback d (cb c)
  -- Open the port for input
  case pid of
    PortIdReal p -> openPort d p name
    PortIdVirtual -> openVirtualPort d name
  pure (InputState d c)

inputRelease :: InputState -> IO ()
inputRelease (InputState d c) = do
  -- Switch to non-blocking reads first (in case the rest throws an exception)
  atomically (setTEvent c)
  -- Close the port for input
  closePort d
  -- Remove our callback
  cancelCallback d

inputCb :: InputQueue -> TEvent -> InputCallback
inputCb q e d w = void (eventWriteDQueueUnlifted q e (d, w))

type ConnResult a = Either Error a

inputConn :: MonadResource m => InputConfig -> Int -> InputDevice -> ConduitT Void (Int, (Double, [Word8])) m ()
inputConn icfg cap d = do
  q <- newDQueueUnlifted cap
  bracketP (inputAcquire icfg (inputCb q) d) inputRelease (\(InputState _ c) -> sourceDQueue q c)

newtype OutputState = OutputState
  { ostDevice :: OutputDevice
  }

outputAcquire :: Config -> OutputDevice -> IO OutputState
outputAcquire (Config name pid) d = do
  -- Open the port for output
  case pid of
    PortIdReal p -> openPort d p name
    PortIdVirtual -> openVirtualPort d name
  pure (OutputState d)

outputRelease :: OutputState -> IO ()
outputRelease (OutputState d) = do
  closePort d
  closeDevice d

outputConduit :: MonadIO m => OutputState -> ConduitT [Word8] Void m ()
outputConduit (OutputState d) = forever $ do
  mpair <- await
  case mpair of
    Nothing -> pure ()
    Just m -> liftIO (sendMessage d m)

outputConn :: MonadResource m => Config -> OutputDevice -> ConduitT [Word8] Void m ()
outputConn cfg d = bracketP (outputAcquire cfg d) outputRelease outputConduit
