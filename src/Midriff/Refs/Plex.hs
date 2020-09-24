module Midriff.Refs.Plex
  ( Plex
  , newPlex
  , plexLockedOpen
  , plexLockedClose
  , plexLockedWith
  , plexLockedLookup
  , plexLockedCloseAll
  , plexUnlockedOpen
  , plexUnlockedClose
  , plexUnlockedLookup
  , plexUnlockedCloseAll
  , plexMember
  , plexKeys
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, release)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Midriff.Refs.Classes (LockRef (..), NewRef (..), ReadWriteRef (..))
import Midriff.Resource (Manager, managedAllocate)
import UnliftIO.Exception (finally, onException)

removeMapVal :: (Hashable a, Eq a) => a -> HashMap a b -> (HashMap a b, Maybe b)
removeMapVal a m =
  let v = HM.lookup a m
      m' = case v of { Nothing -> m; Just _ -> HM.delete a m }
  in (m', v)

newtype Plex r a b = Plex { unPlex :: r (HashMap a (ReleaseKey, b)) }

newPlex :: (NewRef r m, Functor m) => m (Plex r a b)
newPlex = fmap Plex (newRef HM.empty)

plexLockedOpen :: (LockRef r m, MonadResource m, MonadUnliftIO m, Hashable a, Eq a) => Plex r a b -> a -> Manager b -> (b -> m ()) -> m Bool
plexLockedOpen (Plex mref) a manB post =
  modifyLockRef mref $ \m -> do
    if HM.member a m
      then pure (m, False)
      else do
        (rk, b) <- managedAllocate manB
        onException (post b) (release rk)
        pure (HM.insert a (rk, b) m, True)

plexUnlockedOpen :: (ReadWriteRef r m, MonadResource m, Hashable a, Eq a) => Plex r a b -> a -> Manager b -> m (Maybe b)
plexUnlockedOpen (Plex mref) a manB = do
  m <- readRef mref
  if HM.member a m
    then pure Nothing
    else do
      p@(_, b) <- managedAllocate manB
      writeRef mref (HM.insert a p m)
      pure (Just b)

plexLockedClose :: (LockRef r m, MonadUnliftIO m, Hashable a, Eq a) => Plex r a b -> a -> (b -> m ()) -> m Bool
plexLockedClose (Plex mref) a pre =
  modifyLockRef mref $ \m -> do
    let (m', mrk) = removeMapVal a m
    case mrk of
      Nothing -> pure (m, False)
      Just (rk, b) -> finally (pre b $> (m', True)) (release rk)

plexUnlockedClose :: (ReadWriteRef r m, MonadUnliftIO m, Hashable a, Eq a) => Plex r a b -> a -> m Bool
plexUnlockedClose (Plex mref) a = do
  m <- readRef mref
  let (m', mrk) = removeMapVal a m
  case mrk of
    Nothing -> pure False
    Just (rk, _) -> do
      release rk
      writeRef mref m'
      pure True

releaseAll :: [ReleaseKey] -> IO ()
releaseAll rks =
  case rks of
    [] -> pure ()
    (rk:rks') -> finally (release rk) (releaseAll rks')

plexLockedCloseAll :: (LockRef r m, MonadIO m) => Plex r a b -> m ()
plexLockedCloseAll (Plex mref) =
  modifyLockRef_ mref $ \m ->
    let elems = HM.elems m
        rks = fmap fst elems
    in liftIO (releaseAll rks) $> HM.empty

plexUnlockedCloseAll :: (ReadWriteRef r m, MonadIO m) => Plex r a b -> m ()
plexUnlockedCloseAll (Plex mref) = do
  m <- readRef mref
  let elems = HM.elems m
      rks = fmap fst elems
  liftIO (releaseAll rks)
  writeRef mref HM.empty

plexLockedWith :: LockRef r m => Plex r a b -> m c -> m c
plexLockedWith (Plex mref) = withLockRef mref . const

plexLockedLookup :: (LockRef r m, Applicative m, Hashable a, Eq a) => Plex r a b -> a -> (b -> m c) -> m (Maybe c)
plexLockedLookup (Plex mref) a f =
  withLockRef mref $ \m ->
    case HM.lookup a m of
      Nothing -> pure Nothing
      Just (_, b) -> fmap Just (f b)

plexUnlockedLookup :: (ReadWriteRef r m, Functor m, Hashable a, Eq a) => Plex r a b -> a -> m (Maybe b)
plexUnlockedLookup (Plex mref) a = fmap (fmap snd . HM.lookup a) (readRef mref)

plexMember :: (ReadWriteRef r m, Functor m, Hashable a, Eq a) => Plex r a b -> a -> m Bool
plexMember (Plex mref) a = fmap (HM.member a) (readRef mref)

plexKeys :: (ReadWriteRef r m, Functor m) => Plex r a b -> m [a]
plexKeys (Plex mref) = fmap HM.keys (readRef mref)
