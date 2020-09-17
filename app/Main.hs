module Main (main) where

import Control.Concurrent.Async (wait)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT, runConduit, (.|))
import Data.Conduit.List (sourceList)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import LittleRIO (runRIO, withResourceMap)
import Midriff.Config (PortConfig (..), PortId (..))
import Midriff.Connect (manageOutputC, openOutputDevice)
import Midriff.Msg
import Midriff.Process (encodeMsgC, msgDelayC)
import Midriff.Resource (managedAsync)
import Midriff.Time (timeDeltaFromFracSecs)
import Sound.RtMidi (currentApi, listPorts)

noteOn :: Int -> Int -> MidiMsg
noteOn k v = ParsedMidiMsg (MidiChanVoice (ChanVoiceMsg 1 (ChanVoiceNoteOn k v)))

songEvents :: [MidiEvent]
songEvents =
  let arp0 = take 12 $ cycle [0x51, 0x55, 0x58]
      arp1 = take 12 $ cycle [0x51, 0x56, 0x5a]
      arp2 = take 12 $ cycle [0x50, 0x53, 0x58]
      song = cycle (arp0 ++ arp1 ++ arp2 ++ arp0)
      td = timeDeltaFromFracSecs (0.5 :: Double)
  in [MidiEvent td (noteOn k 0x7f) | k <- take 240 song]

songC :: MonadIO m => ConduitT () (VS.Vector Word8) m ()
songC = sourceList songEvents .| msgDelayC .| encodeMsgC

program :: (MonadResource m, MonadUnliftIO m) => m ()
program = do
  outDev <- openOutputDevice Nothing
  api <- currentApi outDev
  liftIO (print api)
  ports <- listPorts outDev
  liftIO (print ports)
  let outputC = manageOutputC (PortConfig "midriff-exe" PortIdVirtual) outDev
  (_, res) <- managedAsync (runConduit (songC .| outputC))
  liftIO (wait res)

main :: IO ()
main = withResourceMap (flip runRIO program)
