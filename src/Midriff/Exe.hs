module Midriff.Exe
  ( MonadExe
  , SimpleExe
  , runExe
  , runSimpleExe
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource)
import LittleRIO (RIO, ResourceMap, runRIO, withResourceMap)

type MonadExe m = (MonadUnliftIO m, MonadResource m)
type SimpleExe a = RIO ResourceMap a

runExe :: (ResourceMap -> env) -> RIO env a -> IO a
runExe f program = withResourceMap (\rm -> runRIO (f rm) program)

runSimpleExe :: SimpleExe a -> IO a
runSimpleExe = runExe id
