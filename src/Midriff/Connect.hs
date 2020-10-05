module Midriff.Connect
  ( InputState (..)
  , InputCallback
  , InputMsg
  , InputQueue
  , QueueInputState (..)
  , OutputState (..)
  , OutputMsg
  , openInputDevice
  , openOutputDevice
  , manageInput
  , manageOutput
  , manageQueueInput
  , manageDelayedOutput
  , queueInputC
  , outputC
  , manageQueueInputC
  , manageOutputC
  , consumeQueueInput
  , produceOutput
  , produceDelayedOutput
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (atomically)
import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import Data.Conduit (ConduitT, await)
import qualified Data.Vector.Storable as VS
import Data.Void (Void)
import Data.Word (Word8)
import Midriff.Callback (Callback, newCallbackIO, runCallback)
import Midriff.Config (DeviceConfig (..), Ignores (..), InputConfig (..), PortConfig (..), PortId (..))
import Midriff.CQueue (CQueue, QueueEvent (..), closeCQueue, newCQueue, readCQueue, sourceCQueue, writeCQueue)
import Midriff.RateLim (RateLim, closeRateLim, newRateLim, readRateLim, writeRateLim)
import Midriff.Resource (Manager, managedAsyncIO, managedConduit, mkManager)
import Midriff.Time (TimeDelta)
import Sound.RtMidi (InputDevice, OutputDevice, cancelCallback, closePort, createInput, createOutput, defaultInput,
                     defaultOutput, ignoreTypes, openPort, openVirtualPort, sendMessage, setCallback)

type InputCallback = Double -> VS.Vector Word8 -> IO ()
type InputCloseCallback = IO ()
type InputMsg = (Double, VS.Vector Word8)
type InputQueue = CQueue (Double, VS.Vector Word8)
type OutputMsg = VS.Vector Word8

internalInputCb :: InputQueue -> InputCallback
internalInputCb cq fracSecs bytes = atomically (void (writeCQueue (fracSecs, bytes) cq))

data InputState = InputState
  { istDevice :: !InputDevice
  , istCloseCb :: !InputCloseCallback
  }

openInputDevice :: MonadIO m => Maybe DeviceConfig -> m InputDevice
openInputDevice mpc = do
  case mpc of
    Nothing -> defaultInput
    -- Empty queue is fine for this, since we set a callback
    Just (DeviceConfig api client) -> createInput api client 0

openOutputDevice :: MonadIO m => Maybe DeviceConfig -> m OutputDevice
openOutputDevice mpc = do
  case mpc of
    Nothing -> defaultOutput
    Just (DeviceConfig api client) -> createOutput api client

acquireInput :: InputConfig -> InputDevice -> InputCallback -> InputCloseCallback -> IO InputState
acquireInput (InputConfig (PortConfig name pid) migs) dev cb closeCb = do
  -- Set our ignored message types
  case migs of
    Nothing -> pure ()
    Just (Ignores x y z) -> ignoreTypes dev x y z
  -- Set our callback to enqueue to our own ring buffer
  setCallback dev cb
  -- Open the port for input
  case pid of
    PortIdReal pnum -> openPort dev pnum name
    PortIdVirtual -> openVirtualPort dev name
  pure (InputState dev closeCb)

releaseInput :: InputState -> IO ()
releaseInput (InputState dev closeCb) = do
  -- Ensure we invoke the close callback last
  flip finally closeCb $ do
    -- Close the port for input
    closePort dev
    -- Remove our callback
    cancelCallback dev

manageInput :: InputConfig -> InputDevice -> InputCallback -> InputCloseCallback -> Manager InputState
manageInput icfg dev cb closeCb = mkManager (acquireInput icfg dev cb closeCb) releaseInput

data QueueInputState = QueueInputState
  { qisCQueue :: CQueue InputMsg
  , qisInputState :: InputState
  }

acquireQueueInput :: InputConfig -> InputDevice -> Int -> IO QueueInputState
acquireQueueInput icfg dev cap = do
  cq <- atomically (newCQueue cap)
  let cb = internalInputCb cq
      closeCb = atomically (closeCQueue cq)
  is <- acquireInput icfg dev cb closeCb
  pure (QueueInputState cq is)

releaseQueueInput :: QueueInputState -> IO ()
releaseQueueInput = releaseInput . qisInputState

manageQueueInput :: InputConfig -> InputDevice -> Int -> Manager QueueInputState
manageQueueInput icfg dev cap = mkManager (acquireQueueInput icfg dev cap) releaseQueueInput

queueInputC :: MonadIO m => QueueInputState -> ConduitT () (QueueEvent InputMsg) m ()
queueInputC (QueueInputState cq _) = sourceCQueue cq

manageQueueInputC :: MonadResource m => InputConfig -> InputDevice -> Int -> ConduitT () (QueueEvent InputMsg) m ()
manageQueueInputC icfg dev cap = managedConduit (manageQueueInput icfg dev cap) queueInputC

consumeQueueInput :: MonadResource m => QueueInputState -> Callback (Int, InputMsg) -> m (ReleaseKey, Async ())
consumeQueueInput (QueueInputState queue _) callback = managedAsyncIO go where
  go = do
    mayMsg <- atomically (readCQueue queue)
    case mayMsg of
      Nothing -> pure ()
      Just msg -> runCallback callback msg *> go

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

outputC :: MonadIO m => OutputState -> ConduitT OutputMsg Void m ()
outputC (OutputState dev) = loop where
  loop = do
    mbytes <- await
    case mbytes of
      Nothing -> pure ()
      Just bytes -> sendMessage dev bytes *> loop

manageOutputC :: MonadResource m => PortConfig -> OutputDevice -> ConduitT OutputMsg Void m ()
manageOutputC cfg dev = managedConduit (manageOutput cfg dev) outputC

produceOutput :: OutputState -> Callback OutputMsg
produceOutput (OutputState dev) = newCallbackIO go where
  go outMsg = sendMessage dev outMsg

data DelayedOutputState = DelayedOutputState
  { dosAsync :: !(Async ())
  , dosRateLim :: !(RateLim OutputMsg)
  , dosOutputState :: !OutputState
  }

acquireDelayedOutput :: PortConfig -> OutputDevice -> Int -> TimeDelta -> Callback (QueueEvent OutputMsg) -> IO DelayedOutputState
acquireDelayedOutput cfg dev cap period preSend = do
  os <- acquireOutput cfg dev
  rl <- newRateLim cap period
  let send = newCallbackIO (\(QueueEvent _ _ a) -> sendMessage dev a)
  as <- async (readRateLim rl (preSend <> send))
  pure (DelayedOutputState as rl os)

releaseDelayedOutput :: DelayedOutputState -> IO ()
releaseDelayedOutput (DelayedOutputState as rl os) =
  finally (closeRateLim rl) (finally (cancel as) (releaseOutput os))

manageDelayedOutput :: PortConfig -> OutputDevice -> Int -> TimeDelta -> Callback (QueueEvent OutputMsg) -> Manager DelayedOutputState
manageDelayedOutput cfg dev cap period preSend = mkManager (acquireDelayedOutput cfg dev cap period preSend) releaseDelayedOutput

produceDelayedOutput :: DelayedOutputState -> Callback OutputMsg
produceDelayedOutput (DelayedOutputState _ rl _) = newCallbackIO (void . writeRateLim rl)
