{-# LANGUAGE DeriveAnyClass #-}

module Midriff.Multiplex
  ( InPlexHandle (..)
  , InPlexResponse (..)
  , InPlexResponseData (..)
  , InPlexResponseQueue
  , InPlex
  , inPlexOpen
  , inPlexClose
  , inPlexIsOpen
  , inPlexListOpen
  , manageInPlex
  , manageQueueInPlex
  , OutPlex
  , outPlexOpen
  , outPlexSend
  , outPlexClose
  , outPlexIsOpen
  , outPlexListOpen
  , manageOutPlex
  ) where

import Control.Concurrent.STM (atomically)
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, release)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import GHC.Generics (Generic)
import Midriff.Config (InputConfig, PortConfig)
import Midriff.Connect (OutputState (..), manageInput, manageOutput)
import Midriff.CQueue (CQueue, closeCQueue, newCQueue, writeCQueue)
import Midriff.Resource (Manager, managedAllocate, mkManager)
import Sound.RtMidi (InputDevice, OutputDevice, sendMessage)
import UnliftIO.Exception (finally, onException)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)
import UnliftIO.MVar (MVar, newMVar, putMVar, takeMVar, withMVar)

data InPlexHandle a = InPlexHandle
  { iphInput :: !(a -> Double -> VS.Vector Word8 -> IO ())
  , iphOpen :: !(a -> IO ())
  , iphClose :: !(a -> IO ())
  }

data InPlexResponseData =
    InPlexResponseInput !Double !(VS.Vector Word8)
  | InPlexResponseOpen
  | InPlexResponseClose
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data InPlexResponse a = InPlexResponse
  { iprName :: !a
  , iprData :: !InPlexResponseData
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Thread-safe. Locks on operations and keeps locks for open/close callbacks.
data InPlex a = InPlex
  { ipHandle :: !(InPlexHandle a)
  , ipMap :: !(MVar (HashMap a ReleaseKey))
  }

type InPlexResponseQueue a = CQueue (InPlexResponse a)

inPlexResponseQueueHandle :: InPlexResponseQueue a -> InPlexHandle a
inPlexResponseQueueHandle queue =
  let onMsg d a = void (atomically (writeCQueue (InPlexResponse a d) queue))
      onInp a t b = onMsg (InPlexResponseInput t b) a
  in InPlexHandle
      { iphInput = onInp
      , iphOpen = onMsg InPlexResponseOpen
      , iphClose = onMsg InPlexResponseClose
      }

removeMapVal :: (Hashable a, Eq a) => a -> HashMap a b -> (HashMap a b, Maybe b)
removeMapVal a m =
  let v = HM.lookup a m
      m' = case v of { Nothing -> m; Just _ -> HM.delete a m }
  in (m', v)

newInPlex :: InPlexHandle a -> IO (InPlex a)
newInPlex handle = fmap (InPlex handle) (newMVar HM.empty)

newQueueInPlex :: Int -> IO (InPlexResponseQueue a, InPlex a)
newQueueInPlex cap = do
  queue <- atomically (newCQueue cap)
  mref <- newMVar HM.empty
  let handle = inPlexResponseQueueHandle queue
      ip = InPlex handle mref
  pure (queue, ip)

inPlexOpen :: (MonadResource m, MonadUnliftIO m, Hashable a, Eq a) => InPlex a -> a -> InputConfig -> InputDevice -> m Bool
inPlexOpen (InPlex handle mref) a icfg dev = do
  let cb = iphInput handle a
      closeCb = iphClose handle a
  m <- liftIO (takeMVar mref)
  if HM.member a m
    then liftIO (putMVar mref m) $> False
    else flip onException (putMVar mref m) $ do
      (rk, _) <- managedAllocate (manageInput icfg dev cb closeCb)
      onException (liftIO (iphOpen handle a)) (release rk)
      putMVar mref (HM.insert a rk m)
      pure True

inPlexIsOpen :: (MonadUnliftIO m, Hashable a, Eq a) => InPlex a -> a -> m Bool
inPlexIsOpen (InPlex _ mref) a = withMVar mref (pure . HM.member a)

inPlexListOpen :: MonadUnliftIO m => InPlex a -> m [a]
inPlexListOpen (InPlex _ mref) = withMVar mref (pure . HM.keys)

inPlexClose :: (MonadResource m, Hashable a, Eq a) => InPlex a -> a -> m Bool
inPlexClose (InPlex _ mref) a = do
  m <- liftIO (takeMVar mref)
  let (m', mrk) = removeMapVal a m
  case mrk of
    Nothing -> liftIO (putMVar mref m') $> False
    Just rk ->
      liftIO (finally (release rk $> True) (putMVar mref m'))

releaseAll :: [ReleaseKey] -> IO ()
releaseAll rks =
  case rks of
    [] -> pure ()
    (rk:rks') -> finally (release rk) (releaseAll rks')

inPlexCloseAll :: MonadIO m => InPlex a -> m ()
inPlexCloseAll (InPlex _ mref) = liftIO $ do
  m <- takeMVar mref
  finally (releaseAll (HM.elems m)) (putMVar mref HM.empty)

manageInPlex :: InPlexHandle a -> Manager (InPlex a)
manageInPlex handle = mkManager (newInPlex handle) inPlexCloseAll

manageQueueInPlex :: Int -> Manager (InPlexResponseQueue a, InPlex a)
manageQueueInPlex cap = mkManager (newQueueInPlex cap) (\(q, ip) -> finally (inPlexCloseAll ip) (atomically (closeCQueue q)))

-- | Note: Not thread-safe. This is to avoid having to lock every time we want to send a message.
newtype OutPlex a = OutPlex
  { opMap :: IORef (HashMap a (ReleaseKey, OutputState))
  }

newOutPlex :: IO (OutPlex a)
newOutPlex = fmap OutPlex (newIORef HM.empty)

outPlexOpen :: (MonadResource m, Hashable a, Eq a) => OutPlex a -> a -> PortConfig -> OutputDevice -> m Bool
outPlexOpen (OutPlex mref) a pcfg dev = do
  m <- readIORef mref
  if HM.member a m
    then pure False
    else  do
      (rk, os) <- managedAllocate (manageOutput pcfg dev)
      writeIORef mref (HM.insert a (rk, os) m)
      pure True

outPlexClose :: (MonadIO m, Hashable a, Eq a) => OutPlex a -> a -> m Bool
outPlexClose (OutPlex mref) a = do
  m <- readIORef mref
  let (m', mrk) = removeMapVal a m
  case mrk of
    Nothing -> pure False
    Just (rk, _) ->
      liftIO (finally (release rk $> True) (writeIORef mref m'))

outPlexIsOpen :: (MonadIO m, Hashable a, Eq a) => OutPlex a -> a -> m Bool
outPlexIsOpen (OutPlex mref) a = fmap (HM.member a) (readIORef mref)

outPlexListOpen :: MonadIO m => OutPlex a -> m [a]
outPlexListOpen (OutPlex mref) = fmap HM.keys (readIORef mref)

outPlexSend :: (MonadIO m, Hashable a, Eq a) => OutPlex a -> a -> VS.Vector Word8 -> m Bool
outPlexSend (OutPlex mref) a bytes = do
  m <- readIORef mref
  case HM.lookup a m of
    Nothing -> pure False
    Just (_, OutputState dev) -> sendMessage dev bytes $> True

outPlexCloseAll :: OutPlex a -> IO ()
outPlexCloseAll (OutPlex mref) = do
  m <- readIORef mref
  finally (releaseAll (fmap fst (HM.elems m))) (writeIORef mref HM.empty)

manageOutPlex :: Manager (OutPlex a)
manageOutPlex = mkManager newOutPlex outPlexCloseAll
