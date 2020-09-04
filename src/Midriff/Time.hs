module Midriff.Time
  ( TimeDelta
  , timeDeltaFromFracSecs
  , timeDeltaFromNanos
  , timeDeltaToFracSecs
  , timeDeltaToNanos
  , diffTimeDelta
  , MonoTime
  , monoTimeToFracSecs
  , monoTimeToNanos
  , monoTimeFromFracSecs
  , monoTimeFromNanos
  , currentMonoTime
  , addMonoTime
  , diffMonoTime
  , threadDelayDelta
  , awaitDelta
  ) where

import Control.Concurrent (threadDelay)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

-- Time delta in nanoseconds since last event
newtype TimeDelta = TimeDelta { unTimeDelta :: Word64 } deriving (Eq, Show, Ord, Num)

timeDeltaFromFracSecs :: RealFrac a => a -> TimeDelta
timeDeltaFromFracSecs d = TimeDelta (round (1000000000 * toRational d))

timeDeltaFromNanos :: Integral a => a -> TimeDelta
timeDeltaFromNanos = TimeDelta . fromIntegral

timeDeltaToFracSecs :: RealFrac a => TimeDelta -> a
timeDeltaToFracSecs (TimeDelta n) = fromIntegral n / 1000000000

timeDeltaToNanos :: TimeDelta -> Word64
timeDeltaToNanos = unTimeDelta

diffTimeDelta :: TimeDelta -> TimeDelta -> Maybe TimeDelta
diffTimeDelta (TimeDelta big) (TimeDelta small) =
  if big <= small
    then Nothing
    else Just (TimeDelta (big - small))

newtype MonoTime = MonoTime { unMonoTime :: Word64 } deriving (Eq, Show, Ord, Num)

monoTimeFromFracSecs :: RealFrac a => a -> MonoTime
monoTimeFromFracSecs d = MonoTime (round (1000000000 * toRational d))

monoTimeFromNanos :: Integral a => a -> MonoTime
monoTimeFromNanos = MonoTime . fromIntegral

monoTimeToFracSecs :: RealFrac a => MonoTime -> a
monoTimeToFracSecs (MonoTime n) = fromIntegral n / 1000000000

monoTimeToNanos :: MonoTime -> Word64
monoTimeToNanos = unMonoTime

currentMonoTime :: IO MonoTime
currentMonoTime = fmap MonoTime getMonotonicTimeNSec

addMonoTime :: MonoTime -> TimeDelta -> MonoTime
addMonoTime (MonoTime mt) (TimeDelta td) = MonoTime (mt + td)

diffMonoTime :: MonoTime -> MonoTime -> Maybe TimeDelta
diffMonoTime (MonoTime end) (MonoTime start) =
  if end <= start
    then Nothing
    else Just (TimeDelta (end - start))

threadDelayDelta :: TimeDelta -> IO ()
threadDelayDelta (TimeDelta td) = threadDelay (fromIntegral (div td 1000))

awaitDelta :: MonoTime -> TimeDelta -> IO MonoTime
awaitDelta m t = do
  let target = addMonoTime m t
  cur <- currentMonoTime
  maybe (pure ()) threadDelayDelta (diffMonoTime target cur)
  pure target
