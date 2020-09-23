module Midriff.Async where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (atomically)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import Midriff.Connect (InputMsg, OutputMsg, OutputState (..), QueueInputState (..))
import Midriff.CQueue (readCQueue)
import Midriff.Handle (Handle, newHandleIO, runHandle)
import Midriff.Resource (managedAsyncIO)
import Sound.RtMidi (sendMessage)

consumeQueueInput :: MonadResource m => QueueInputState -> Handle (Int, InputMsg) -> m (ReleaseKey, Async ())
consumeQueueInput (QueueInputState queue _) handle = managedAsyncIO go where
  go = do
    mayMsg <- atomically (readCQueue queue)
    case mayMsg of
      Nothing -> pure ()
      Just msg -> runHandle handle msg *> go

produceOutput :: OutputState -> Handle OutputMsg
produceOutput (OutputState dev) = newHandleIO go where
  go outMsg = sendMessage dev outMsg
