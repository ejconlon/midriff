module Midriff.Async where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (atomically)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import Midriff.Connect (InputMsg, InputState (..), OutputMsg, OutputState (..))
import Midriff.CQueue (readCQueue)
import Midriff.Handle (Handle, newHandleIO, runHandle)
import Midriff.Resource (managedAsyncIO)
import Sound.RtMidi (sendMessage)

consumeInput :: MonadResource m => InputState -> Handle (Int, InputMsg) -> m (ReleaseKey, Async ())
consumeInput (InputState _ queue) handle = managedAsyncIO go where
  go = do
    mayMsg <- atomically (readCQueue queue)
    case mayMsg of
      Nothing -> pure ()
      Just msg -> runHandle handle msg

produceOutput :: OutputState -> Handle OutputMsg
produceOutput (OutputState dev) = newHandleIO go where
  go outMsg = sendMessage dev outMsg
