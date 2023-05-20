module Midriff.Resource
  ( Manager
  , managerNew
  , managerNewU
  , managerBracket
  , managerAllocate
  , Ref
  , refPure
  , refNew
  , refNewWith
  , refGate
  , refAwait
  , refClose
  , refControl
  , refPeek
  , refUse
  , RefM
  , refRead
  , refMayRead
  , refRun
  )
where

import Control.Monad.Fix (mfix)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket, finally, mask_, onException)
import Control.Monad (ap, unless, void, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (..), askUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.Bitraversable (bitraverse)
import Midriff.Gate (Gate (..), gateAwait)
import Midriff.Control (Control (..))

-- | Pair of (acquire, release) functions that can be used for bracket functions.
-- In spirit, this is like 'Data.Acquire.Acquire' but lacks some functionality
-- because it must remain splittable for use across 'bracket', 'bracketP', and 'allocate'.
data Manager a where
  Manager :: !(z -> IO a) -> !(IO z) -> !(z -> IO ()) -> Manager a

instance Functor Manager where
  fmap f (Manager use acq rel) = Manager (fmap f . use) acq rel

managerMapM :: (a -> IO b) -> Manager a -> Manager b
managerMapM f (Manager use acq rel) = Manager (use >=> f) acq rel

-- | Make a 'Manager' from a pair of (acquire, release) functions.
managerNew :: IO a -> (a -> IO ()) -> Manager a
managerNew = Manager pure

-- | Make a 'Manager' from a pair of (acquire, release) functions in an unliftable monad.
managerNewU :: MonadUnliftIO m => m a -> (a -> m ()) -> m (Manager a)
managerNewU acq rel = do
  UnliftIO run <- askUnliftIO
  pure (Manager pure (run acq) (run . rel))

-- | 'bracket' using the 'Manager' functions.
managerBracket :: MonadUnliftIO m => Manager a -> (a -> m b) -> m b
managerBracket (Manager use acq rel) f = do
  UnliftIO run <- askUnliftIO
  liftIO (bracket (liftIO acq) (liftIO . rel) (use >=> run . f))

-- | 'allocate' using the 'Manager' functions.
managerAllocate :: MonadResource m => Manager a -> m (ReleaseKey, a)
managerAllocate (Manager use acq rel) = allocate acq rel >>= bitraverse pure (liftIO . use)

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

refPure :: a -> IO (Ref a)
refPure = fmap (`Ref` Nothing) . newTVarIO . XOpen

refNew :: MonadResource m => Manager a -> m (Ref a)
refNew (Manager use alloc free) = fmap snd $ flip allocate (void . refClose) $ do
  z <- alloc
  a <- onException (use z) (free z)
  var <- newTVarIO (XOpen a)
  pure (Ref var (Just (free z)))

refNewWith :: MonadResource m => Manager a -> (Control -> a -> IO b) -> m (Ref b)
refNewWith (Manager use alloc free) f = fmap snd $ flip allocate (void . refClose) $ do
  mfix $ \ref -> do
    z <- alloc
    b <- onException (use z >>= f (refControl ref)) (free z)
    var <- newTVarIO (XOpen b)
    pure (Ref var (Just (free z)))

refGate :: Ref a -> STM Gate
refGate = fmap xGate . readTVar . refVar

refAwait :: Ref a -> STM ()
refAwait = refGate >=> gateAwait

refControl :: Ref a -> Control
refControl r = Control (refGate r) (refClose r)

-- | Release the ref, returning True if this waiting until close,
-- False if early return due to other thread releasing.
refCloseNoWait :: Ref a -> IO Bool
refCloseNoWait (Ref var mfin) = mask_ $
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

refClose :: Ref a -> IO Bool
refClose r = do
  closer <- refCloseNoWait r
  unless closer (atomically (refAwait r))
  pure closer

refPeek :: Ref a -> STM (Maybe a)
refPeek (Ref var _) = do
  x <- readTVar var
  case x of
    XOpen a -> pure (Just a)
    XLocked -> retry
    _ -> pure Nothing

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
