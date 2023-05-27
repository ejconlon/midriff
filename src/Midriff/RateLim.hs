module Midriff.RateLim
  ( RateLim
  , rlNew
  , rlNewIO
  , rlAwait
  , rlIsReady
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar)
import Midiot.Time (MonoTime, TimeDelta)

data RateLim = RateLim
  { rlPeriod :: !TimeDelta
  , rlCap :: !(TVar Int)
  , rlLast :: !(TVar MonoTime)
  }
  deriving stock (Eq)

rlNew :: TimeDelta -> Int -> STM RateLim
rlNew _per _cap = error "TODO"

rlNewIO :: TimeDelta -> Int -> IO RateLim
rlNewIO _per _cap = error "TODO"

rlAwait :: RateLim -> STM ()
rlAwait = error "TODO"

rlIsReady :: RateLim -> STM Bool
rlIsReady = error "TODO"

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
