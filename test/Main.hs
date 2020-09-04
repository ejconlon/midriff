module Main (main) where

import Test.Midriff.SimpleTest (testSimple)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "Midriff" [testSimple])
