module Midriff.Process where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Conduit (ConduitT, await, yield)
import Midriff.Time (TimeDelta, awaitDelta, currentMonoTime)

delay :: MonadIO m => (i -> (TimeDelta, o)) -> ConduitT i o m ()
delay f = go where
  go = do
    m <- liftIO currentMonoTime
    loop m
  loop m = do
    mi <- await
    case mi of
      Nothing -> pure ()
      Just i -> do
        let (td, o) = f i
        m' <- liftIO (awaitDelta m td)
        yield o
        loop m'

-- msgDelay :: MonadIO m => ConduitT MidiEvent MidiMsg
