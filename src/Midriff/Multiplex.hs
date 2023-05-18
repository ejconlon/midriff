{-# LANGUAGE DeriveAnyClass #-}

module Midriff.Multiplex
  ( InPlexHandle (..)
  , InPlexResponse (..)
  , InPlexResponseData (..)
  , InPlexResponseQueue
  , InPlex
  , inPlexOpen
  , inPlexClose
  , inPlexMember
  , inPlexKeys
  , manageInPlex
  , manageQueueInPlex
  , OutPlex
  , outPlexOpen
  , outPlexClose
  , outPlexSend
  , outPlexSendMany
  , outPlexMember
  , outPlexKeys
  , manageOutPlex
  )
where

import Control.Concurrent.STM (atomically)
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import GHC.Generics (Generic)
import Midriff.CQueue (CQueue, closeCQueue, newCQueue, writeCQueue)
import Midriff.Config (InputConfig, PortConfig)
import Midriff.Connect (InputState, OutputState (..), manageInput, manageOutput)
import Midriff.Refs.Plex
  ( Plex
  , newPlex
  , plexKeys
  , plexLockedClose
  , plexLockedCloseAll
  , plexLockedOpen
  , plexMember
  , plexUnlockedClose
  , plexUnlockedCloseAll
  , plexUnlockedLookup
  , plexUnlockedOpen
  )
import Midriff.Refs.XVar (XVar)
import Midriff.Resource (Manager, mkManager)
import Sound.RtMidi (InputDevice, OutputDevice, sendMessage)
import UnliftIO.Exception (finally)
import UnliftIO.IORef (IORef)

data InPlexHandle a = InPlexHandle
  { iphInput :: !(a -> Double -> VS.Vector Word8 -> IO ())
  , iphOpen :: !(a -> IO ())
  , iphClose :: !(a -> IO ())
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data InPlexResponseData
  = InPlexResponseInput !Double !(VS.Vector Word8)
  | InPlexResponseOpen
  | InPlexResponseClose
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data InPlexResponse a = InPlexResponse
  { iprName :: !a
  , iprData :: !InPlexResponseData
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Thread-safe. Locks on operations and keeps locks for open/close callbacks.
data InPlex a = InPlex
  { ipHandle :: !(InPlexHandle a)
  , ipPlex :: !(Plex XVar a InputState)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

type InPlexResponseQueue a = CQueue (InPlexResponse a)

inPlexResponseQueueHandle :: InPlexResponseQueue a -> InPlexHandle a
inPlexResponseQueueHandle queue =
  let onMsg d a = void (atomically (writeCQueue (InPlexResponse a d) queue))
      onInp a t b = onMsg (InPlexResponseInput t b) a
  in  InPlexHandle
        { iphInput = onInp
        , iphOpen = onMsg InPlexResponseOpen
        , iphClose = onMsg InPlexResponseClose
        }

newInPlex :: InPlexHandle a -> IO (InPlex a)
newInPlex handle = fmap (InPlex handle) newPlex

newQueueInPlex :: Int -> IO (InPlexResponseQueue a, InPlex a)
newQueueInPlex cap = do
  queue <- atomically (newCQueue cap)
  let handle = inPlexResponseQueueHandle queue
  ip <- newInPlex handle
  pure (queue, ip)

inPlexOpen :: (MonadResource m, MonadUnliftIO m, Hashable a) => InPlex a -> a -> InputConfig -> InputDevice -> m Bool
inPlexOpen (InPlex handle plex) a icfg dev = do
  let cb = iphInput handle a
      openCb = iphOpen handle a
      closeCb = iphClose handle a
      man = manageInput icfg dev cb closeCb
  plexLockedOpen plex a man (const (liftIO openCb))

inPlexMember :: (MonadIO m, Hashable a) => InPlex a -> a -> m Bool
inPlexMember (InPlex _ plex) = plexMember plex

inPlexKeys :: MonadIO m => InPlex a -> m [a]
inPlexKeys (InPlex _ plex) = plexKeys plex

inPlexClose :: (MonadUnliftIO m, Hashable a) => InPlex a -> a -> m Bool
inPlexClose (InPlex _ plex) a = plexLockedClose plex a (const (pure ()))

inPlexCloseAll :: InPlex a -> IO ()
inPlexCloseAll (InPlex _ plex) = plexLockedCloseAll plex

manageInPlex :: InPlexHandle a -> Manager (InPlex a)
manageInPlex handle = mkManager (newInPlex handle) inPlexCloseAll

manageQueueInPlex :: Int -> Manager (InPlexResponseQueue a, InPlex a)
manageQueueInPlex cap = mkManager (newQueueInPlex cap) (\(q, ip) -> finally (inPlexCloseAll ip) (atomically (closeCQueue q)))

-- | Note: Not thread-safe. This is to avoid having to lock every time we want to send a message.
newtype OutPlex a = OutPlex
  { opPlex :: Plex IORef a OutputState
  }
  deriving newtype (NFData)

newOutPlex :: IO (OutPlex a)
newOutPlex = fmap OutPlex newPlex

outPlexOpen :: (MonadResource m, Hashable a) => OutPlex a -> a -> PortConfig -> OutputDevice -> m (Maybe OutputState)
outPlexOpen (OutPlex plex) a pcfg dev = do
  let man = manageOutput pcfg dev
  plexUnlockedOpen plex a man

outPlexClose :: (MonadIO m, Hashable a) => OutPlex a -> a -> m Bool
outPlexClose (OutPlex plex) = plexUnlockedClose plex

outPlexMember :: (MonadIO m, Hashable a) => OutPlex a -> a -> m Bool
outPlexMember (OutPlex plex) = plexMember plex

outPlexKeys :: MonadIO m => OutPlex a -> m [a]
outPlexKeys (OutPlex plex) = plexKeys plex

outPlexSend :: (MonadIO m, Hashable a) => OutPlex a -> a -> VS.Vector Word8 -> m Bool
outPlexSend (OutPlex plex) a bytes = do
  mos <- plexUnlockedLookup plex a
  case mos of
    Nothing -> pure False
    Just (OutputState dev) -> liftIO (sendMessage dev bytes) $> True

outPlexSendMany :: (MonadIO m, Hashable a, Foldable f) => OutPlex a -> a -> f (VS.Vector Word8) -> m Bool
outPlexSendMany (OutPlex plex) a msgs = do
  mos <- plexUnlockedLookup plex a
  case mos of
    Nothing -> pure False
    Just (OutputState dev) -> liftIO (for_ msgs (sendMessage dev)) $> True

outPlexCloseAll :: OutPlex a -> IO ()
outPlexCloseAll (OutPlex plex) = plexUnlockedCloseAll plex

manageOutPlex :: Manager (OutPlex a)
manageOutPlex = mkManager newOutPlex outPlexCloseAll
