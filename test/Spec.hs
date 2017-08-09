module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

skipsSuite :: TestTree
skipsSuite = testGroup "0three rec poly TEST"
              [ testGroup "Test of --skips-- function" $
                  [testCase "problem 1" $ m3and5 1000 @?= 233168
                  ,testCase "problem 2" $ foundfibo 8 @?= 10
                  ,testCase "problem 3" $ maximumPrime 600851475143 @?= 6857
                  ]
              ]

main = defaultMain skipsSuite

