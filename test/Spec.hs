module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

skipsSuite :: TestTree
skipsSuite = testGroup "0three rec poly TEST"
              [ testGroup "Test of --skips-- function" $
                  [testCase "problem 1" $ m3and5 1000 @?= 233168
                  ,testCase "problem 2" $ fibonacci 8 @?= 44
                  ]
              ]

main = defaultMain skipsSuite

