module Midriff.Conn where

import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT)
import Data.Void (Void)
import Midriff.Msg (MidiEvent, decodeMsg)

inputConn :: MonadResource m => ConduitT Void MidiEvent m ()
inputConn = undefined
