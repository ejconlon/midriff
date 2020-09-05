module Midriff.Resource
  ( Manager
  , mkManager
  , managedBracket
  , managedConduit
  , managedAllocate
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.Conduit (ConduitT, bracketP)
import UnliftIO.Exception (bracket)

data Manager a = Manager
  { mgrAcquire :: !(IO a)
  , mgrRelease :: !(a -> IO ())
  }

mkManager :: IO a -> (a -> IO ()) -> Manager a
mkManager = Manager

managedBracket :: MonadUnliftIO m => Manager a -> (a -> m b) -> m b
managedBracket (Manager acq rel) = bracket (liftIO acq) (liftIO . rel)

managedConduit :: MonadResource m => Manager a -> (a -> ConduitT i o m r) -> ConduitT i o m r
managedConduit (Manager acq rel) = bracketP acq rel

managedAllocate :: MonadResource m => Manager a -> m (ReleaseKey, a)
managedAllocate (Manager acq rel) = allocate acq rel
