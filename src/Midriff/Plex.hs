module Midriff.Plex
  ( Plex
  , plexNew
  , plexNewU
  , plexOpen
  , plexClose
  , plexCloseAll
  , plexPeek
  , plexLookup
  , plexFold
  , plexFor_
  , plexTrim
  )
where

import Control.Concurrent.STM (atomically)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Midriff.Gate (Gate (..))
import Midriff.Latch (latchAwait, latchGate)
import Midriff.Lock (Lock, lockEscalate, lockNew, lockPeek, lockRead)
import Midriff.Resource (Manager, Ref, refNew, refReleaseWait, refReleaseWaitAll)

data Plex a b = Plex
  { plexFn :: !(a -> IO (Manager b))
  , plexLock :: !(Lock (Map a (Ref b)))
  }

plexNew :: (a -> IO (Manager b)) -> IO (Plex a b)
plexNew fn = fmap (Plex fn) (lockNew Map.empty)

plexNewU :: MonadUnliftIO m => (a -> m (Manager b)) -> m (Plex a b)
plexNewU fn = askRunInIO >>= \run -> liftIO (plexNew (run . fn))

-- | Open a ref. Returns True if allocated new.
plexOpen :: (MonadResource m, MonadUnliftIO m, Ord a) => a -> Plex a b -> m (Ref b, Bool)
plexOpen a (Plex fn lock) = askRunInIO >>= liftIO . lockEscalate lock check . edit
 where
  check m = case Map.lookup a m of
    Nothing -> pure (Left m)
    Just r ->
      atomically $
        latchGate r >>= \case
          GateClosed -> pure (Left m)
          GateClosing -> Left m <$ latchAwait r
          GateOpen -> pure (Right (r, False))
  edit run m = do
    mgr <- fn a
    r <- run (refNew mgr)
    pure ((r, True), Map.insert a r m)

-- | Close a ref and remove it from the map. Returns True if the key was found in the map.
plexClose :: Ord a => a -> Plex a b -> IO Bool
plexClose a (Plex _ lock) = lockEscalate lock check edit
 where
  check m = case Map.lookup a m of
    Nothing -> pure (Right False)
    Just r -> Left m <$ refReleaseWait r
  edit m = pure (True, Map.delete a m)

plexCloseAll :: Plex a b -> IO [a]
plexCloseAll (Plex _ lock) = lockEscalate lock check edit
 where
  check m =
    if Map.null m
      then pure (Right [])
      else Left (Map.keys m) <$ refReleaseWaitAll (Map.elems m)
  edit as = pure (as, Map.empty)

plexPeek :: Plex a b -> IO (Map a (Ref b))
plexPeek = atomically . lockPeek . plexLock

plexLookup :: Ord a => a -> Plex a b -> IO (Maybe (Ref b))
plexLookup a = fmap (Map.lookup a) . plexPeek

plexFold :: Plex a b -> c -> (c -> a -> Ref b -> IO c) -> IO c
plexFold (Plex _ lock) z f = lockRead lock (foldM (\c (a, r) -> f c a r) z . Map.toList)

plexFor_ :: Plex a b -> (a -> Ref b -> IO ()) -> IO ()
plexFor_ (Plex _ lock) f = lockRead lock (traverse_ (uncurry f) . Map.toList)

-- | Best-effort deletion of closing/closed refs.
plexTrim :: Ord a => Plex a b -> IO [a]
plexTrim (Plex _ lock) = lockEscalate lock check pure
 where
  check m = do
    let ps = Map.toList m
    p@(as, _) <- foldM plus ([], Map.empty) ps
    pure $
      if null as
        then Right []
        else Left p
  plus (as, m') (a, r) = atomically $ do
    g <- latchGate r
    case g of
      GateClosed -> pure (a : as, m')
      GateClosing -> (a : as, m') <$ latchAwait r
      GateOpen -> pure (as, Map.insert a r m')
