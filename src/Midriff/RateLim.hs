module Midriff.RateLim
  ( RateLim
  , rlNew
  , rlNewIO
  , rlAwait
  , rlIsReady
  -- , rlWrite
  -- , rlLatch
  -- , rlRead
  -- , rlTryRead
  )
where

import Control.Concurrent.STM (STM)
import Midriff.Time (MonoTime, TimeDelta, awaitDelta)
import Midriff.Ring (Next)
import Midriff.Latch (Latch)
import Midriff.Gate (Gate)
import Control.Concurrent.STM.TVar (TVar)

data RateLim = RateLim
  { rlPeriod :: !TimeDelta
  , rlCap :: !(TVar Int)
  , rlLast :: !(TVar MonoTime)
  }
  deriving stock (Eq)

rlNew :: TimeDelta -> Int -> STM RateLim
rlNew per cap = undefined

rlNewIO :: TimeDelta -> Int -> IO RateLim
rlNewIO per cap = undefined

rlAwait :: RateLim -> STM ()
rlAwait = undefined

rlIsReady :: RateLim -> STM Bool
rlIsReady = undefined

-- rlRead (RateLim period cq) f = liftIO $ loop (0 :: Int) minBound
--  where
--   loop !n lastTime = do
--     m <- atomically (readCQueue cq)
--     case m of
--       Nothing -> pure ()
--       Just (i, a) -> do
--         curTime <- awaitDelta lastTime period
--         runCallback f (QueueEvent n i a)
--         loop (succ n) curTime

-- rlRun :: RateLim -> IO a -> IO a
-- rlRun (RateLim per capVar lastVar) act = go
--  where
--    go = do
--     g <- atomically (readCQueue cq)
--     case m of
--       Nothing -> pure ()
--       Just (i, a) -> do
--         curTime <- awaitDelta lastTime period
--         runCallback f (QueueEvent n i a)
--         loop (succ n) curTime
