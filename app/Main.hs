module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import LittleRIO (runRIO, withResourceMap)
import Midriff.Conn (manageOutputDevice)
import Midriff.Resource (managedAllocate)
import Sound.RtMidi (listPorts)

program :: MonadResource m => m ()
program = do
  (_, outDev) <- managedAllocate (manageOutputDevice Nothing)
  ports <- liftIO (listPorts outDev)
  liftIO (print ports)

main :: IO ()
main = withResourceMap (flip runRIO program)
