module Main (main) where

import Data.Foldable (for_)
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

main :: IO ()
main = defaultMain $ testGroup "Midriff" $
  [ testWord14
  , testSplitWord8
  ]
