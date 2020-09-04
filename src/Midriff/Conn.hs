module Midriff.Conn where

import Control.Monad.Trans.Resource (MonadResource)
import Data.Void (Void)
import Midriff.Msg (MidiEvent, decodeMsg)
import Data.Conduit (ConduitT)

inputConn :: MonadResource m => ConduitT Void MidiEvent m ()
inputConn = undefined
