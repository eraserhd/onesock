
module Main where

import Test.HUnit
import System.Random
import System.IO
import System.Exit
import System.Directory
import System.Process
import System.Environment (getArgs)
import System.Console.GetOpt
import OneSock.DB.Scans
import OneSock.ImageUtil
import Control.Monad
import Data.UUID
import qualified Graphics.GD as GD

tests = TestList [
          testsForOnesockDBScans
        ]

data Mode = Test | DoScan | StoreScan
data Flag = ModeFlag Mode

usageHeader = "Usage: OneSock [OPTION...]"
options =
  [ Option ['s']  ["scan"]        (NoArg (ModeFlag DoScan))     "scan and store to database"
  , Option ['S']  ["store-scan"]  (NoArg (ModeFlag StoreScan))  "store scan(s) to scan database"
  , Option []     ["test"]        (NoArg (ModeFlag Test))       "run unit tests"
  ]

findMode :: [Flag] -> Mode
findMode fs =
  let
    isModeFlag (ModeFlag m) = True
    isModeFlag _            = False
    ms = filter isModeFlag fs
  in
    case ms of
      []           -> error "No mode argument supplied"
      [ModeFlag m] -> m
      _            -> error "Only one mode argument can be supplied"

doScan :: DB -> IO ()
doScan db = do
  id <- randomIO :: IO UUID
  tmpDir <- getTemporaryDirectory
  let tiffFile = tmpDir ++ "/" ++ show id ++ ".tiff"
  let pngFile = tmpDir ++ "/" ++ show id ++ ".png"
  rc <- system $ "scanimage --format=tiff --resolution=300 >" ++ tiffFile
  when (rc /= ExitSuccess) $ error "scanimage failed (is sane-utils installed?)"
  rc <- system $ "convert -scale 800 " ++ tiffFile ++ " " ++ pngFile
  when (rc /= ExitSuccess) $ error "convert failed (is imagemagick installed?)"
  bitmap <- GD.loadPngFile pngFile >>= fromGD
  removeFile tiffFile
  removeFile pngFile
  let scan = Scan{scanId=id, scanBitmap=bitmap}
  storeScan db scan
  putStrLn $ show id

doStoreScan :: DB -> FilePath -> IO ()
doStoreScan db filename = do
  bitmap <- GD.loadPngFile filename >>= fromGD
  id <- randomIO :: IO UUID
  let scan = Scan{scanId=id, scanBitmap=bitmap}
  storeScan db scan
  putStrLn $ filename ++ "=" ++ show id

main :: IO ()
main = do
  args <- getArgs
  let (os,as,errs) = getOpt Permute options args
  when (errs /= []) $
    ioError $ userError $ concat errs ++ usageInfo usageHeader options
  let mode = findMode os
  case mode of
    Test -> do
      runTestTT tests
      return ()
    DoScan -> do
      db <- getDefaultDBPath >>= initDB
      doScan db
      
    StoreScan -> do
      db <- getDefaultDBPath >>= initDB
      case as of
        [] -> ioError $ userError $ "No scan files were supplied"
        _  -> mapM_ (doStoreScan db) as
  
