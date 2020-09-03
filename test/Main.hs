module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Midriff.SimpleTest (testSimple)

main :: IO ()
main = defaultMain (testGroup "Midriff" [testSimple])
