module Main (main) where

import Control.Monad.IO.Class (liftIO)
import LittleRIO (HasResourceMap, RIO, runRIO, withResourceMap)
import Midriff.Conn (allocateOutputDevice)
import Sound.RtMidi (listPorts)

program :: HasResourceMap env => RIO env ()
program = do
  (_, d) <- allocateOutputDevice Nothing
  ports <- liftIO (listPorts d)
  liftIO (print ports)

main :: IO ()
main = withResourceMap (flip runRIO program)
