module Midriff.Exe
  ( runExe
  , runSimpleExe
  ) where

import LittleRIO (ResourceMap, RIO, runRIO, withResourceMap)

runExe :: (ResourceMap -> env) -> RIO env a -> IO a
runExe f program = withResourceMap (\rm -> runRIO (f rm) program)

runSimpleExe :: RIO ResourceMap a -> IO a
runSimpleExe = runExe id
