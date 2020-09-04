module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Midriff.Conn (withOutputDevice)
import Sound.RtMidi (listPorts)

main :: IO ()
main = withOutputDevice Nothing $ \d -> do
  ports <- liftIO (listPorts d)
  print ports
