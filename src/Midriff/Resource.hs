module Midriff.Resource
  ( Ref
  , refPure
  , refNew
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

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket, finally, mask_)
import Control.Monad (ap, unless, void, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, register, release)
import Data.Acquire (Acquire, allocateAcquire)
import Midriff.Control (Control (..))
import Midriff.Gate (Gate (..), gateAwait)

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

refNew :: MonadResource m => Acquire a -> m (Ref a)
refNew acq = do
  (k, a) <- allocateAcquire acq
  var <- liftIO (newTVarIO (XOpen a))
  let ref = Ref var (Just (release k))
  _ <- register (void (refClose ref))
  pure ref

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
      Just a -> let (RefM z) = f a in z (\mb q -> k mb (q >> r))

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
