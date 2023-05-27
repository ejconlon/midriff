module Main (main) where

import Data.Foldable (for_)
import Midiot.Time (timeDeltaFromFracSecs, timeDeltaFromNanos)
import Midriff.Freq (TimeUnit (..), countFromFrac, countPerPeriod, freqFromFrac, scaledUnit, singlePeriod)
import Midriff.Word (joinSplitWord8, joinWord14, lsbSplitWord8, lsbWord14, msbSplitWord8, msbWord14)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testWord14 :: TestTree
testWord14 = testCase "Word14" $ do
  for_ [minBound .. maxBound] $ \w -> do
    let msb = msbWord14 w
        lsb = lsbWord14 w
        v = joinWord14 msb lsb
    v @?= w

testSplitWord8 :: TestTree
testSplitWord8 = testCase "Split Word8" $ do
  for_ [minBound .. maxBound] $ \w -> do
    let msb = msbSplitWord8 w
        lsb = lsbSplitWord8 w
        v = joinSplitWord8 msb lsb
    v @?= w

testPeriod :: TestTree
testPeriod = testCase "Period" $ do
  let f = freqFromFrac (10.0 :: Double) TimeUnitSecs
      p = timeDeltaFromFracSecs (0.1 :: Double)
  singlePeriod f @?= p

testCount :: TestTree
testCount = testCase "Count" $ do
  let t1 = 440000.0 :: Double
      t2 = 440.0 :: Double
      t3 = 0.44 :: Double
      f1 = freqFromFrac t1 TimeUnitSecs
      f2 = freqFromFrac t2 TimeUnitMillis
      f3 = freqFromFrac t3 TimeUnitMicros
      s1 = scaledUnit TimeUnitSecs
      s2 = scaledUnit TimeUnitMillis
      s3 = scaledUnit TimeUnitMicros
      e1 = countFromFrac t1
      e2 = countFromFrac t2
      e3 = countFromFrac t3
  countPerPeriod f1 s1 @?= e1
  countPerPeriod f1 s2 @?= e2
  countPerPeriod f1 s3 @?= e3
  countPerPeriod f2 s1 @?= e1
  countPerPeriod f2 s2 @?= e2
  countPerPeriod f2 s3 @?= e3
  countPerPeriod f3 s1 @?= e1
  countPerPeriod f3 s2 @?= e2
  countPerPeriod f3 s3 @?= e3
  let p = timeDeltaFromNanos (2273 :: Int)
  singlePeriod f1 @?= p
  singlePeriod f2 @?= p
  singlePeriod f3 @?= p

main :: IO ()
main =
  defaultMain $
    testGroup
      "Midriff"
      [ testWord14
      , testSplitWord8
      , testPeriod
      , testCount
      ]
