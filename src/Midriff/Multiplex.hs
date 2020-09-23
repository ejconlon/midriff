{-# LANGUAGE DeriveAnyClass #-}

module Midriff.Multiplex
  ( InPlexHandle (..)
  , InPlexMsg (..)
  , InPlexMsgData (..)
  , InPlexQueue
  , InPlex
  , inPlexOpen
  , inPlexClose
  , manageInPlex
  , manageQueueInPlex
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
import Midriff.Config (InputConfig)
import Midriff.Connect (manageInput)
import Midriff.DQueue (DQueue, writeDQueue)
import Midriff.Resource (Manager, managedAllocate, mkManager)
import Sound.RtMidi (InputDevice)
import UnliftIO.Exception (finally, onException)
import UnliftIO.MVar (MVar, newMVar, putMVar, takeMVar)

data InPlexHandle a = InPlexHandle
  { iphInput :: !(a -> Double -> VS.Vector Word8 -> IO ())
  , iphOpen :: !(a -> IO ())
  , iphClose :: !(a -> IO ())
  }

data InPlexMsgData =
    InPlexMsgInput !Double !(VS.Vector Word8)
  | InPlexMsgOpen
  | InPlexMsgClose
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data InPlexMsg a = InPlexMsg
  { ipmName :: !a
  , ipmData :: !InPlexMsgData
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data InPlex a = InPlex
  { ipHandle :: !(InPlexHandle a)
  , ipMap :: !(MVar (HashMap a ReleaseKey))
  }

type InPlexQueue a = DQueue (InPlexMsg a)

inPlexQueueHandle :: InPlexQueue a -> InPlexHandle a
inPlexQueueHandle queue =
  let onMsg d a = void (atomically (writeDQueue (InPlexMsg a d) queue))
      onInp a t b = onMsg (InPlexMsgInput t b) a
  in InPlexHandle
      { iphInput = onInp
      , iphOpen = onMsg InPlexMsgOpen
      , iphClose = onMsg InPlexMsgClose
      }

removeMapVal :: (Hashable a, Eq a) => a -> HashMap a b -> (HashMap a b, Maybe b)
removeMapVal a m =
  let v = HM.lookup a m
      m' = case v of { Nothing -> m; Just _ -> HM.delete a m }
  in (m', v)

newInPlex :: MonadIO m => InPlexHandle a -> m (InPlex a)
newInPlex handle = fmap (InPlex handle) (newMVar HM.empty)

newQueueInPlex :: MonadIO m => InPlexQueue a -> m (InPlex a)
newQueueInPlex = newInPlex . inPlexQueueHandle

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
manageInPlex handle = mkManager (newInPlex handle) (inPlexCloseAll)

manageQueueInPlex :: InPlexQueue a -> Manager (InPlex a)
manageQueueInPlex = manageInPlex . inPlexQueueHandle
