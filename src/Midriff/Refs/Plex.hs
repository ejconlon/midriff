module Midriff.Refs.Plex
  ( Plex
  , newPlex
  ) where

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

plexLockedWith :: LockRef r m => Plex r a b -> m c -> m c
plexLockedWith (Plex mref) = withLockRef mref . const

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
