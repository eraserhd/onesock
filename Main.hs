
module Main where

import Control.Monad
import Data.Time.Clock
import Data.UUID
import qualified Graphics.GD as GD
import GUI (runGUI)
import ImageUtil
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Process
import System.Random
import ScanDB
import qualified Test.HUnit as T

data Mode = DoScan | StoreScan | RunGUI
data Flag = ModeFlag Mode

usageHeader = "Usage: OneSock [OPTION...]"
options =
  [ Option ['s']  ["scan"]        (NoArg (ModeFlag DoScan))     "scan and store to database"
  , Option ['S']  ["store-scan"]  (NoArg (ModeFlag StoreScan))  "store scan(s) to scan database"
  ]

findMode :: [Flag] -> Mode
findMode fs =
  let
    isModeFlag (ModeFlag m) = True
    isModeFlag _            = False
    ms = filter isModeFlag fs
  in
    case ms of
      []           -> RunGUI
      [ModeFlag m] -> m
      _            -> error "Only one mode argument can be supplied"

cmdScan :: DB -> IO ()
cmdScan db = do
  id <- randomIO :: IO UUID
  tmpDir <- getTemporaryDirectory
  let tiffFile = tmpDir ++ "/" ++ show id ++ ".tiff"
  let pngFile = tmpDir ++ "/" ++ show id ++ ".png"
  rc <- system $ "scanimage --format=tiff --resolution=300 >" ++ tiffFile
  when (rc /= ExitSuccess) $ error "scanimage failed (is sane-utils installed?)"
  rc <- system $ "convert -scale 800 " ++ tiffFile ++ " " ++ pngFile
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

main :: IO ()
main = do
  args <- getArgs
  let (os,as,errs) = getOpt Permute options args
  when (errs /= []) $
    ioError $ userError $ concat errs ++ usageInfo usageHeader options
  case findMode os of
    DoScan -> do
      db <- getDefaultDBPath >>= initDB
      cmdScan db
    StoreScan -> do
      db <- getDefaultDBPath >>= initDB
      case as of
        [] -> ioError $ userError $ "No scan files were supplied"
        _  -> mapM_ (cmdStoreScan db) as
    RunGUI -> runGUI
