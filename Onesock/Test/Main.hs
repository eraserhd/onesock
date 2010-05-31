
module Main where

import Test.HUnit
import System.Environment (getArgs)

import qualified Onesock.DB.Scans as ScanDB (tests)

tests = TestList [
          ScanDB.tests 
        ]

main = runTestTT tests
