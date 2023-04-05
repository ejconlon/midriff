module Midriff.Process where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Conduit (ConduitT, await, yield)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Midriff.Msg (MidiEvent (..), MidiMsg, decodeEvent, encodeMsg)
import Midriff.Time (TimeDelta, awaitDelta, currentMonoTime)

{- | Given a function that extracts time and contents from inputs,
 delays the current thread by those times and emits those contents.
-}
delayC :: MonadIO m => (i -> (TimeDelta, o)) -> ConduitT i o m ()
delayC f = liftIO currentMonoTime >>= loop
 where
  loop m = do
    mi <- await
    case mi of
      Nothing -> pure ()
      Just i -> do
        let (td, o) = f i
        m' <- liftIO (awaitDelta m td)
        yield o
        loop m'

-- | A conduit that decodes midi events from lower-level callback payloads.
decodeEventC :: Monad m => ConduitT (Double, VS.Vector Word8) MidiEvent m ()
decodeEventC = CC.map (uncurry decodeEvent)

-- | A conduit that delays according to event timestamps.
msgDelayC :: MonadIO m => ConduitT MidiEvent MidiMsg m ()
msgDelayC = delayC (\(MidiEvent td mp) -> (td, mp))

-- | A conduit that renders midi messages to output payloads.
encodeMsgC :: Monad m => ConduitT MidiMsg (VS.Vector Word8) m ()
encodeMsgC = CC.map encodeMsg
