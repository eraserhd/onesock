module Actions (cmdScan, cmdStoreScan) where

import Control.Monad (when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.UUID (UUID)
import qualified Graphics.GD as GD
import ImageUtil (fromGD, withTestBitmapFile)
import ScanDB
import System.Exit (ExitCode(ExitSuccess))
import System.Directory (removeFile, getTemporaryDirectory)
import System.Process (system)
import System.Random (randomIO)
import qualified Test.HUnit as T

cmdScan :: DB -> IO ()
cmdScan db = do
  id <- randomIO :: IO UUID
  tmpDir <- getTemporaryDirectory
  let tiffFile = tmpDir ++ "/" ++ show id ++ ".tiff"
  let pngFile = tmpDir ++ "/" ++ show id ++ ".png"
  rc <- system $ "scanimage --format=tiff --resolution=300 >" ++ tiffFile
  when (rc /= ExitSuccess) $ error "scanimage failed (is sane-utils installed?)"
  rc <- system $ "convert " ++ tiffFile ++ " " ++ pngFile
  when (rc /= ExitSuccess) $ error "convert failed (is imagemagick installed?)"
  storeScanFromFile db pngFile
  removeFile tiffFile
  removeFile pngFile
  putStrLn $ show id

storeScanFromFile :: DB -> FilePath -> IO UUID
storeScanFromFile db filename = do
  bitmap <- GD.loadPngFile filename >>= fromGD
  id <- randomIO :: IO UUID
  now <- getCurrentTime
  let scan = Scan{scanId=id, scanTime=now, scanBitmap=bitmap}
  storeScan db scan
  return id

test_storeScanFromFileUsesCurrentTime = do
  withTestDb $ \db -> do
  withTestBitmapFile $ \fn -> do
  now <- getCurrentTime
  id <- storeScanFromFile db fn
  scan <- lookupScan db id
  T.assertBool "store time is too far off" $ abs (diffUTCTime (scanTime scan) now) < 5.0
  T.assertEqual "scanId scan" id (scanId scan)

cmdStoreScan :: DB -> FilePath -> IO ()
cmdStoreScan db filename = do
  id <- storeScanFromFile db filename
  putStrLn $ filename ++ "=" ++ show id
