module Actions (
  UI, setStatus, notifyScanAdded,

  cmdScan, cmdStoreScan,
  ) where

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

class UI a where
  setStatus :: a -> String -> IO ()
  notifyScanAdded :: a -> UUID -> IO ()

cmdScan :: UI ui => DB -> ui -> IO ()
cmdScan db ui = do
  id <- randomIO :: IO UUID
  tmpDir <- getTemporaryDirectory
  let tiffFile = tmpDir ++ "/" ++ show id ++ ".tiff"
  let pngFile = tmpDir ++ "/" ++ show id ++ ".png"
  setStatus ui "Scanning..."
  rc <- system $ "scanimage --format=tiff --resolution=300 >" ++ tiffFile
  (case rc of
    ExitSuccess -> do
      setStatus ui "Converting..."
      rc <- system $ "convert " ++ tiffFile ++ " " ++ pngFile
      case rc of
        ExitSuccess -> do
          storeScanFromFile db pngFile
          removeFile tiffFile
          removeFile pngFile
          setStatus ui $ "Done scanning " ++ show id
        _ -> setStatus ui "convert failed (is imagemagick installed?)"
    _ -> setStatus ui "scanimage failed (is sane-utils installed?)")

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

cmdStoreScan :: UI ui => DB -> ui -> FilePath -> IO ()
cmdStoreScan db ui filename = do
  id <- storeScanFromFile db filename
  putStrLn $ filename ++ "=" ++ show id
