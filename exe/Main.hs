module Main (main) where

import Control.Concurrent.Async (wait)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Conduit (ConduitT, runConduit, (.|))
import Data.Conduit.List (sourceList)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import LittleRIO (RIO, ResourceMap, runRIO, withResourceMap)
import Midriff.Config (PortConfig (..), PortId (..))
import Midriff.Connect (manageOutputC, openOutputDevice)
import Midriff.Msg (Channel (..), MidiEvent (..), Note (..), Velocity (..), noteOn)
import Midriff.Process (encodeMsgC, msgDelayC)
import Midriff.Resource (managedAsync)
import Midriff.Time (timeDeltaFromFracSecs)
import Sound.RtMidi (currentApi, listPorts)

songEvents :: [MidiEvent]
songEvents =
  let arp0 = take 12 $ cycle [0x51, 0x55, 0x58]
      arp1 = take 12 $ cycle [0x51, 0x56, 0x5a]
      arp2 = take 12 $ cycle [0x50, 0x53, 0x58]
      song = cycle (arp0 ++ arp1 ++ arp2 ++ arp0)
      td = timeDeltaFromFracSecs (0.5 :: Double)
  in  [MidiEvent td (noteOn (Channel 1) (Note k) (Velocity 0x7f)) | k <- take 36 song]

songC :: MonadIO m => ConduitT () (VS.Vector Word8) m ()
songC = sourceList songEvents .| msgDelayC .| encodeMsgC

program :: RIO ResourceMap ()
program = do
  outDev <- openOutputDevice Nothing
  api <- liftIO (currentApi outDev)
  liftIO (print api)
  ports <- liftIO (listPorts outDev)
  liftIO (print ports)
  let outputC = manageOutputC (PortConfig "midriff-exe" PortIdVirtual) outDev
  (_, res) <- managedAsync (runConduit (songC .| outputC))
  liftIO (wait res)

main :: IO ()
main = withResourceMap (`runRIO` program)
