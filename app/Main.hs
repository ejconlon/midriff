module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import LittleRIO (runRIO, withResourceMap)
import Midriff.Conn (manageOutputDevice)
import Midriff.Resource (managedAllocate)
import Sound.RtMidi (currentApi, listPorts)

program :: MonadResource m => m ()
program = do
  (_, outDev) <- managedAllocate (manageOutputDevice Nothing)
  api <- liftIO (currentApi outDev)
  liftIO (print api)
  ports <- liftIO (listPorts outDev)
  liftIO (print ports)

main :: IO ()
main = withResourceMap (flip runRIO program)
