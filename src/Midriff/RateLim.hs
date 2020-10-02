{-# LANGUAGE DeriveAnyClass #-}

module Midriff.RateLim
  ( RateLim
  , writeRateLim
  , closeRateLim
  , readRateLimIO
  , readRateLim
  , newRateLim
  ) where

import Control.Concurrent.STM (atomically)
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import GHC.Generics (Generic)
import Midriff.CQueue (CQueue, WriteResult, closeCQueue, newCQueue, readCQueue, writeCQueue)
import Midriff.Time (TimeDelta, awaitDelta)

data RateLim a = RateLim
  { rlCQueue :: !(CQueue a)
  , rlPeriod :: !TimeDelta
  } deriving stock (Eq, Generic)
    deriving anyclass (NFData)

writeRateLim :: MonadIO m => RateLim a -> a -> m WriteResult
writeRateLim (RateLim cq _) a = liftIO (atomically (writeCQueue a cq))

closeRateLim :: MonadIO m => RateLim a -> m ()
closeRateLim (RateLim cq _) = liftIO (atomically (closeCQueue cq))

readRateLimIO :: RateLim a -> (Int -> a -> IO ()) -> IO ()
readRateLimIO (RateLim cq period) f = loop minBound where
  loop lastTime = do
    m <- atomically (readCQueue cq)
    case m of
      Nothing -> pure ()
      Just (i, a) -> do
        curTime <- awaitDelta lastTime period
        f i a
        loop curTime

readRateLim :: MonadUnliftIO m => RateLim a -> (Int -> a -> m ()) -> m ()
readRateLim rl f = do
  run <- askRunInIO
  liftIO (readRateLimIO rl (\i a -> run (f i a)))

newRateLim :: MonadIO m => Int -> TimeDelta -> m (RateLim a)
newRateLim cap period = do
  cq <- liftIO (atomically (newCQueue cap))
  pure (RateLim cq period)
