{-# LANGUAGE DeriveAnyClass #-}

module Midriff.Freq
  ( TimeUnit (..)
  , ScaledDelta
  , scaledUnit
  , scaledToTimeDelta
  , scaledFromTimeDelta
  , Count
  , splitCount
  , countFromFrac
  , countToFrac
  , Freq
  , freqFromFrac
  , freqToFrac
  , countPerPeriod
  , singlePeriod
  )
where

import Control.DeepSeq (NFData)
import Data.Ratio ((%))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Midriff.Time (TimeDelta, assertingNonNegative, timeDeltaFromNanos, timeDeltaToNanos)

data TimeUnit
  = TimeUnitNanos
  | TimeUnitMicros
  | TimeUnitMillis
  | TimeUnitSecs
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

data ScaledDelta = ScaledDelta
  { scaledDeltaUnit :: !TimeUnit
  , scaledDeltaValue :: !Word64
  , scaledDeltaRemainder :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

scaleFactor :: TimeUnit -> Word64
scaleFactor unit =
  case unit of
    TimeUnitNanos -> 1
    TimeUnitMicros -> 1000
    TimeUnitMillis -> 1000000
    TimeUnitSecs -> 1000000000

scaledUnit :: TimeUnit -> ScaledDelta
scaledUnit unit = ScaledDelta unit 1 0

scaledToTimeDelta :: ScaledDelta -> TimeDelta
scaledToTimeDelta (ScaledDelta unit value re) =
  timeDeltaFromNanos ((scaleFactor unit * value) + re)

scaledFromTimeDelta :: TimeDelta -> TimeUnit -> ScaledDelta
scaledFromTimeDelta td unit =
  let fac = scaleFactor unit
      ns = timeDeltaToNanos td
      (val, re) = quotRem ns fac
  in  ScaledDelta unit val re

data Count = Count
  { countWhole :: !Word64
  , countPart :: !Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup Count where
  Count w1 p1 <> Count w2 p2 = Count (w1 + w2 + wl) p3
   where
    (wl, p3) = properFraction (p1 + p2)

instance Monoid Count where
  mempty = Count 0 0
  mappend = (<>)

countToRational :: Count -> Rational
countToRational (Count w p) = toRational w + toRational p

splitCount :: Count -> (Word64, Count)
splitCount (Count whole part) = (whole, Count 0 part)

mulCount :: Count -> Word64 -> Double -> Count
mulCount (Count w1 p1) w2 p2 = Count (w1 * w2 + wl) p3
 where
  (wl, p3) = properFraction (fromIntegral w2 * p1 + fromIntegral w1 * p2 + p1 * p2)

countFromFrac :: (Real a, Show a) => a -> Count
countFromFrac frac =
  let rat = toRational (assertingNonNegative frac)
      (whole, partRat) = properFraction rat
      part = fromRational partRat
  in  Count whole part

countToFrac :: Fractional a => Count -> a
countToFrac (Count w p) = fromIntegral w + realToFrac p

countGte :: Count -> Word64 -> Bool
countGte (Count w _) x = w >= x

countSub :: Count -> Word64 -> Maybe Count
countSub (Count w p) x = if w >= x then Just (Count (w - x) p) else Nothing

-- | Frequency is count per unit time.
data Freq = Freq
  { freqCount :: !Count
  , freqScaledDelta :: !ScaledDelta
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

freqFromFrac :: (Real a, Show a) => a -> TimeUnit -> Freq
freqFromFrac frac unit =
  Freq (countFromFrac frac) (ScaledDelta unit 1 0)

freqToFrac :: Freq -> (Double, TimeUnit)
freqToFrac (Freq c (ScaledDelta unit val re)) =
  let fac = scaleFactor unit
      partRat = re % fac
      part = realToFrac partRat
      c' = mulCount c val part
  in  (countToFrac c', unit)

data AccEnv = AccEnv
  { accEnvCount :: !Count
  , accEnvTimeDelta :: !TimeDelta
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

newAccEnv :: Freq -> AccEnv
newAccEnv (Freq c sd) = AccEnv c (scaledToTimeDelta sd)

accStep :: AccEnv -> TimeDelta -> Count
accStep (AccEnv c td) stepTd =
  let ns = timeDeltaToNanos td
      (whole, partNum) = quotRem (timeDeltaToNanos stepTd) ns
      partRat = partNum % ns
      part = realToFrac partRat
  in  mulCount c whole part

-- | Inefficient, use sparingly.
countPerPeriod :: Freq -> ScaledDelta -> Count
countPerPeriod freq sd = accStep (newAccEnv freq) (scaledToTimeDelta sd)

accSinglePeriod :: AccEnv -> TimeDelta
accSinglePeriod (AccEnv c td) =
  let i = round (toRational (timeDeltaToNanos td) / countToRational c) :: Word64
  in  timeDeltaFromNanos i

-- | Also inefficient, use sparingly.
singlePeriod :: Freq -> TimeDelta
singlePeriod = accSinglePeriod . newAccEnv
