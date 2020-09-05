module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT, (.|), runConduit)
import Data.Conduit.List (sourceList)
import Data.Word (Word8)
import LittleRIO (runRIO, withResourceMap)
import Midriff.Config (Config (..), PortId (..))
import Midriff.Conn (manageOutputDevice, outputConn)
import Midriff.Process (encodeParsedC, msgDelayC)
import Midriff.Resource (managedAllocate)
import Midriff.Time (timeDeltaFromFracSecs)
import Sound.RtMidi (currentApi, listPorts)
import Midriff.Msg

noteOn :: Int -> Int -> MidiParsed
noteOn k v = MidiParsed (Right (BasicMsg (MidiChanVoice 1 (ChanVoiceNoteOn k v))))

songEvents :: [MidiEvent]
songEvents =
  let arp0 = take 12 $ cycle [0x51, 0x55, 0x58]
      arp1 = take 12 $ cycle [0x51, 0x56, 0x5a]
      arp2 = take 12 $ cycle [0x50, 0x53, 0x58]
      song = cycle (arp0 ++ arp1 ++ arp2 ++ arp0)
      td = timeDeltaFromFracSecs 1.0
  in [MidiEvent td (noteOn k 0x7f) | k <- take 240 song]

songC :: MonadIO m => ConduitT () [Word8] m ()
songC = sourceList songEvents .| msgDelayC .| encodeParsedC

program :: MonadResource m => m ()
program = do
  (_, outDev) <- managedAllocate (manageOutputDevice Nothing)
  api <- liftIO (currentApi outDev)
  liftIO (print api)
  ports <- liftIO (listPorts outDev)
  liftIO (print ports)
  let conn = outputConn (Config "midriff-exe" PortIdVirtual) outDev
  runConduit (songC .| conn)

main :: IO ()
main = withResourceMap (flip runRIO program)
