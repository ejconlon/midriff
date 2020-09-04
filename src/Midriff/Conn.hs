module Midriff.Conn
  ( ConnResult
  , virtualInputConn
  , virtualOutputConn
  ) where

import Control.Concurrent.STM (atomically)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT, await, bracketP, tryC)
import Data.Void (Void)
import Data.Word (Word8)
import Midriff.TEvent (TEvent, newTEvent, setTEvent)
import Midriff.Unlifted (DQueue, eventWriteDQueueUnlifted, newDQueueUnlifted, sourceDQueue)
import Sound.RtMidi (Error, InputDevice, OutputDevice, cancelCallback, closeDevice, closePort, defaultInput,
                     defaultOutput, openVirtualPort, sendMessage, setCallback)

type InputCallback = Double -> [Word8] -> IO ()

type InputQueue = DQueue (Double, [Word8])

data InputState = InputState
  { istDevice :: !InputDevice
  , istCloseEvent :: !TEvent
  }

virtualInputAcquire :: String -> (TEvent -> InputCallback) -> IO InputState
virtualInputAcquire name cb = do
  c <- atomically newTEvent
  d <- defaultInput
  openVirtualPort d name
  setCallback d (cb c)
  pure (InputState d c)

virtualInputRelease :: InputState -> IO ()
virtualInputRelease (InputState d c) = do
  atomically (setTEvent c)
  cancelCallback d
  closePort d
  closeDevice d

inputCb :: InputQueue -> TEvent -> InputCallback
inputCb q e d w = void (eventWriteDQueueUnlifted q e (d, w))

type ConnResult a = Either Error a

virtualInputConn :: (MonadResource m, MonadUnliftIO m) => String -> Int -> ConduitT Void (Int, (Double, [Word8])) m (ConnResult ())
virtualInputConn name cap = do
  q <- newDQueueUnlifted cap
  tryC (bracketP (virtualInputAcquire name (inputCb q)) virtualInputRelease (\(InputState _ c) -> sourceDQueue q c))

newtype OutputState = OutputState
  { ostDevice :: OutputDevice
  }

virtualOutputAcquire :: String -> IO OutputState
virtualOutputAcquire name = do
  d <- defaultOutput
  openVirtualPort d name
  pure (OutputState d)

virtualOutputRelease :: OutputState -> IO ()
virtualOutputRelease (OutputState d) = do
  closePort d
  closeDevice d

outputConduit :: MonadIO m => OutputState -> ConduitT [Word8] Void m ()
outputConduit (OutputState d) = forever $ do
  mpair <- await
  case mpair of
    Nothing -> pure ()
    Just m -> liftIO (sendMessage d m)

virtualOutputConn :: (MonadResource m, MonadUnliftIO m) => String -> ConduitT [Word8] Void m (ConnResult ())
virtualOutputConn name = tryC (bracketP (virtualOutputAcquire name) virtualOutputRelease outputConduit)
