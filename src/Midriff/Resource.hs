module Midriff.Resource
  ( Manager
  , managerNew
  , managerNewU
  , managerBracket
  , managerAllocate
  , managerAsync
  , managerAsyncIO
  , Ref
  , refPure
  , refNew
  , refRelease
  , refReleaseWait
  , refReleaseAll
  , refReleaseWaitAll
  , refUse
  , RefM
  , refRead
  , refMayRead
  , refRun
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket, finally, mask_)
import Control.Monad (ap, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (..), askRunInIO, askUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.Foldable (toList, traverse_)
import Midriff.Gate (Gate (..))
import Midriff.Latch (RLatch (..), latchAwait)

-- | Pair of (acquire, release) functions that can be used for bracket functions.
-- In spirit, this is like 'Data.Acquire.Acquire' but lacks some functionality
-- because it must remain splittable for use across 'bracket', 'bracketP', and 'allocate'.
data Manager a = Manager
  { mgrAcquire :: !(IO a)
  , mgrRelease :: !(a -> IO ())
  }

-- | Make a 'Manager' from a pair of (acquire, release) functions.
managerNew :: IO a -> (a -> IO ()) -> Manager a
managerNew = Manager

-- | Make a 'Manager' from a pair of (acquire, release) functions in an unliftable monad.
managerNewU :: MonadUnliftIO m => m a -> (a -> m ()) -> m (Manager a)
managerNewU acq rel = do
  UnliftIO run <- askUnliftIO
  pure (Manager (run acq) (run . rel))

-- | 'bracket' using the 'Manager' functions.
managerBracket :: MonadUnliftIO m => Manager a -> (a -> m b) -> m b
managerBracket (Manager acq rel) f = do
  UnliftIO run <- askUnliftIO
  liftIO (bracket (liftIO acq) (liftIO . rel) (run . f))

-- | 'allocate' using the 'Manager' functions.
managerAllocate :: MonadResource m => Manager a -> m (ReleaseKey, a)
managerAllocate (Manager acq rel) = allocate acq rel

-- | 'async' as a resource, calling 'cancel' on release.
managerAsync :: (MonadResource m, MonadUnliftIO m) => m a -> m (Async a)
managerAsync m = do
  run <- askRunInIO
  managerAsyncIO (run m)

-- | 'managedAsync' wrapping an 'IO' action
managerAsyncIO :: MonadResource m => IO a -> m (Async a)
managerAsyncIO m = fmap snd (allocate (async m) cancel)

data X a
  = XOpen !a
  | XLocked
  | XClosing
  | XClosed
  deriving stock (Eq, Ord, Show)

xGate :: X a -> Gate
xGate = \case
  XOpen _ -> GateOpen
  XLocked -> GateOpen
  XClosing -> GateClosing
  XClosed -> GateClosed

data Ref a = Ref
  { refVar :: !(TVar (X a))
  , refFin :: !(Maybe (IO ()))
  }

instance RLatch (Ref a) where
  latchGate = fmap xGate . readTVar . refVar

refPure :: a -> IO (Ref a)
refPure = fmap (`Ref` Nothing) . newTVarIO . XOpen

refNew :: MonadResource m => Manager a -> m (Ref a)
refNew (Manager alloc free) = fmap snd $ flip allocate (void . refRelease) $ do
  a <- alloc
  var <- newTVarIO (XOpen a)
  pure (Ref var (Just (free a)))

-- | Release the ref, returning True if this waiting until close,
-- False if early return due to other thread releasing.
refRelease :: Ref a -> IO Bool
refRelease (Ref var mfin) = mask_ $
  case mfin of
    Nothing -> atomically $ do
      x <- readTVar var
      case x of
        XOpen _ -> True <$ writeTVar var XClosed
        XLocked -> retry
        _ -> pure False
    Just fin -> do
      cleanup <- atomically $ do
        x <- readTVar var
        case x of
          XOpen _ -> True <$ writeTVar var XClosing
          XLocked -> retry
          _ -> pure False
      if cleanup
        then finally (True <$ fin) (atomically (writeTVar var XClosed))
        else pure False

refReleaseWait :: Ref a -> IO ()
refReleaseWait r = refRelease r *> atomically (latchAwait r)

refReleaseAll :: Foldable f => f (Ref b) -> IO ()
refReleaseAll = traverse_ (void . refRelease)

refReleaseWaitAll :: Foldable f => f (Ref b) -> IO ()
refReleaseWaitAll = traverse_ (void . refReleaseWait)

refUse :: Ref a -> (Maybe a -> IO b) -> IO b
refUse (Ref var _) = bracket start end
 where
  start = atomically $ do
    x <- readTVar var
    case x of
      XOpen a -> Just a <$ writeTVar var XLocked
      XLocked -> retry
      _ -> pure Nothing
  end = maybe (pure ()) (atomically . writeTVar var . XOpen)

newtype RefM a = RefM {unRefM :: forall b. (Maybe a -> STM () -> STM (b, STM ())) -> STM (b, STM ())}
  deriving stock (Functor)

instance Applicative RefM where
  pure a = RefM (\k -> k (Just a) (pure ()))
  (<*>) = ap

instance Monad RefM where
  return = pure
  RefM y >>= f = RefM $ \k -> y $ \ma r ->
    case ma of
      Nothing -> k Nothing r
      Just a -> let (RefM z) = f a in z (\mb q -> k mb (q *> r))

refReadWith :: (Maybe a -> Maybe b) -> Ref a -> RefM b
refReadWith f (Ref var _) = RefM $ \k -> do
  x <- readTVar var
  case x of
    XOpen a -> writeTVar var XLocked *> k (f (Just a)) (writeTVar var x)
    XLocked -> retry
    _ -> k (f Nothing) (pure ())

refRead :: Ref a -> RefM a
refRead = refReadWith id

refMayRead :: Ref a -> RefM (Maybe a)
refMayRead = refReadWith Just

refRun :: RefM a -> (Maybe a -> IO b) -> IO b
refRun (RefM y) f = bracket start end middle
 where
  start = atomically (y (curry pure))
  middle (ma, _) = f ma
  end (_, r) = atomically r
