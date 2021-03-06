{-# LANGUAGE DeriveAnyClass #-}

module Midriff.RateLim
  ( RateLim
  , writeRateLim
  , closeRateLim
  , readRateLim
  , newRateLim
  ) where

import Control.Concurrent.STM (atomically)
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO (..))
import GHC.Generics (Generic)
import Midriff.Callback (Callback, runCallback)
import Midriff.CQueue (CQueue, QueueEvent (..), WriteResult, closeCQueue, newCQueue, readCQueue, writeCQueue)
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

readRateLim :: MonadIO m => RateLim a -> Callback (QueueEvent a) -> m ()
readRateLim (RateLim cq period) f = liftIO $ loop (0 :: Int) minBound where
  loop !n lastTime = do
    m <- atomically (readCQueue cq)
    case m of
      Nothing -> pure ()
      Just (i, a) -> do
        curTime <- awaitDelta lastTime period
        runCallback f (QueueEvent n i a)
        loop (succ n) curTime

newRateLim :: MonadIO m => Int -> TimeDelta -> m (RateLim a)
newRateLim cap period = do
  cq <- liftIO (atomically (newCQueue cap))
  pure (RateLim cq period)
