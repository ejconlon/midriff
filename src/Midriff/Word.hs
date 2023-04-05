{-# LANGUAGE DeriveAnyClass #-}

{- | Note that this module exports constructors for 'Word7' and 'Word14' but they're not
 always safe to use! They're only there to allow you to 'coerce' when you know it's safe.
 Otherwise use 'fromIntegral' to ensure the values are brought into range.
-}
module Midriff.Word
  ( msbSplitWord8
  , lsbSplitWord8
  , joinSplitWord8
  , Word7 (..)
  , Word14 (..)
  , msbWord14
  , lsbWord14
  , joinWord14
  )
where

import Control.DeepSeq (NFData)
import Data.Bits (Bits (..))
import Data.Hashable (Hashable)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)

makeWord8 :: Integer -> Word8
makeWord8 i = fromIntegral (mod i 256)

extractWord8 :: Word8 -> Integer
extractWord8 = fromIntegral

msbSplitWord8 :: Word8 -> Word7
msbSplitWord8 w = makeWord7 (shiftR (extractWord8 w) 7)

lsbSplitWord8 :: Word8 -> Word7
lsbSplitWord8 w = makeWord7 (extractWord8 w .&. 0x7F)

joinSplitWord8 :: Word7 -> Word7 -> Word8
joinSplitWord8 msb lsb = makeWord8 (shiftL (extractWord7 msb) 7 .|. extractWord7 lsb)

newtype Word7 = Word7 {getWord7 :: Word8}
  deriving stock (Generic)
  deriving newtype (Enum, Eq, Ord, Integral, Real, Bits)
  deriving anyclass (NFData, Hashable)

makeWord7 :: Integer -> Word7
makeWord7 i = Word7 (fromInteger (mod i 128))

extractWord7 :: Word7 -> Integer
extractWord7 = fromIntegral . getWord7

instance Bounded Word7 where
  minBound = 0
  maxBound = 127

instance Num Word7 where
  a + b = makeWord7 (extractWord7 a + extractWord7 b)
  a - b = makeWord7 (extractWord7 a - extractWord7 b)
  a * b = makeWord7 (extractWord7 a * extractWord7 b)
  abs a = Word7 (abs (getWord7 a))
  negate a = makeWord7 (negate (extractWord7 a))
  signum a = Word7 (signum (getWord7 a))
  fromInteger = makeWord7

instance Show Word7 where
  showsPrec p = showsPrec p . getWord7

newtype Word14 = Word14 {getWord14 :: Word16}
  deriving stock (Generic)
  deriving newtype (Enum, Eq, Ord, Integral, Real, Bits)
  deriving anyclass (NFData, Hashable)

msbWord14 :: Word14 -> Word7
msbWord14 w = makeWord7 (shiftR (extractWord14 w) 7)

lsbWord14 :: Word14 -> Word7
lsbWord14 w = makeWord7 (extractWord14 w .&. 0x7F)

joinWord14 :: Word7 -> Word7 -> Word14
joinWord14 msb lsb = makeWord14 (shiftL (extractWord7 msb) 7 .|. extractWord7 lsb)

makeWord14 :: Integer -> Word14
makeWord14 i = Word14 (fromInteger (mod i 16384))

extractWord14 :: Word14 -> Integer
extractWord14 = fromIntegral . getWord14

instance Bounded Word14 where
  minBound = 0
  maxBound = 16383

instance Num Word14 where
  a + b = makeWord14 (extractWord14 a + extractWord14 b)
  a - b = makeWord14 (extractWord14 a - extractWord14 b)
  a * b = makeWord14 (extractWord14 a * extractWord14 b)
  abs a = Word14 (abs (getWord14 a))
  negate a = makeWord14 (negate (extractWord14 a))
  signum a = Word14 (signum (getWord14 a))
  fromInteger = makeWord14

instance Show Word14 where
  showsPrec p = showsPrec p . getWord14
