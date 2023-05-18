module Midriff.Plex
  ( Plex
  , newPlex
  , plexOpen
  , plexClose
  , plexLookup
  , plexCloseAll
  , plexMember
  , plexKeys
  , plexKeysSet
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, release)
import Control.Exception (finally)
import Midriff.Resource (Manager, managedAllocate)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

removeMapVal :: Ord a => a -> Map a b -> (Map a b, Maybe b)
removeMapVal a m =
  let v = Map.lookup a m
      m' = case v of Nothing -> m; Just _ -> Map.delete a m
  in  (m', v)

-- TODO add a latch
newtype Plex a b = Plex {unPlex :: IORef (Map a (ReleaseKey, b))}

newPlex :: MonadIO m => m (Plex a b)
newPlex = liftIO (fmap Plex (newIORef Map.empty))

plexOpen :: (MonadResource m, Ord a) => Plex a b -> a -> Manager b -> m (Maybe b)
plexOpen (Plex ref) a manB = do
  m <- liftIO (readIORef ref)
  if Map.member a m
    then pure Nothing
    else do
      p@(_, b) <- managedAllocate manB
      liftIO (writeIORef ref (Map.insert a p m))
      pure (Just b)

plexClose :: (MonadIO m, Ord a) => Plex a b -> a -> m Bool
plexClose (Plex ref) a = liftIO $ do
  m <- liftIO (readIORef ref)
  let (m', mrk) = removeMapVal a m
  case mrk of
    Nothing -> pure False
    Just (rk, _) -> liftIO $ do
      release rk
      writeIORef ref m'
      pure True

releaseAll :: [ReleaseKey] -> IO ()
releaseAll rks =
  case rks of
    [] -> pure ()
    (rk : rks') -> finally (release rk) (releaseAll rks')

plexCloseAll :: MonadIO m => Plex a b -> m ()
plexCloseAll (Plex ref) = liftIO $ do
  m <- readIORef ref
  let rks = fmap fst (Map.elems m)
  releaseAll rks
  writeIORef ref Map.empty

plexLookup :: (MonadIO m, Ord a) => Plex a b -> a -> m (Maybe b)
plexLookup (Plex ref) a = liftIO (fmap (fmap snd . Map.lookup a) (readIORef ref))

plexMember :: (MonadIO m, Ord a) => Plex a b -> a -> m Bool
plexMember (Plex ref) a = liftIO (fmap (Map.member a) (readIORef ref))

plexKeys :: (MonadIO m) => Plex a b -> m [a]
plexKeys (Plex ref) = liftIO (fmap Map.keys (readIORef ref))

plexKeysSet :: (MonadIO m) => Plex a b -> m (Set a)
plexKeysSet (Plex ref) = liftIO (fmap Map.keysSet (readIORef ref))
