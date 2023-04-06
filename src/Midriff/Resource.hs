module Midriff.Resource
  ( Manager
  , mkManager
  , mkManagerUnlifted
  , managedBracket
  , managedConduit
  , managedAllocate
  , managedAsync
  , managedAsyncIO
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (..), askRunInIO, askUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.Conduit (ConduitT, bracketP)
import UnliftIO.Exception (bracket)

-- | Pair of (acquire, release) functions that can be used for bracket functions.
-- In spirit, this is like 'Data.Acquire.Acquire' but lacks some functionality
-- because it must remain splittable for use across 'bracket', 'bracketP', and 'allocate'.
data Manager a = Manager
  { mgrAcquire :: !(IO a)
  , mgrRelease :: !(a -> IO ())
  }

-- | Make a 'Manager' from a pair of (acquire, release) functions.
mkManager :: IO a -> (a -> IO ()) -> Manager a
mkManager = Manager

-- | Make a 'Manager' from a pair of (acquire, release) functions in an unliftable monad.
mkManagerUnlifted :: MonadUnliftIO m => m a -> (a -> m ()) -> m (Manager a)
mkManagerUnlifted acq rel = do
  UnliftIO run <- askUnliftIO
  pure (Manager (run acq) (run . rel))

-- | 'bracket' using the 'Manager' functions.
managedBracket :: MonadUnliftIO m => Manager a -> (a -> m b) -> m b
managedBracket (Manager acq rel) = bracket (liftIO acq) (liftIO . rel)

-- | 'bracketP' using the 'Manager' functions.
managedConduit :: MonadResource m => Manager a -> (a -> ConduitT i o m r) -> ConduitT i o m r
managedConduit (Manager acq rel) = bracketP acq rel

-- | 'allocate' using the 'Manager' functions.
managedAllocate :: MonadResource m => Manager a -> m (ReleaseKey, a)
managedAllocate (Manager acq rel) = allocate acq rel

-- | 'async' as a resource, calling 'cancel' on release.
managedAsync :: (MonadResource m, MonadUnliftIO m) => m a -> m (ReleaseKey, Async a)
managedAsync m = do
  run <- askRunInIO
  allocate (async (run m)) cancel

-- | 'managedAsync' wrapping an 'IO' action
managedAsyncIO :: MonadResource m => IO a -> m (ReleaseKey, Async a)
managedAsyncIO m = allocate (async m) cancel
