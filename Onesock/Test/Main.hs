
module Main where

import Test.HUnit
import System.Environment (getArgs)

import qualified Onesock.DB.Image as ImageDB (tests)

tests = TestList [
          ImageDB.tests 
        ]

main = runTestTT tests
