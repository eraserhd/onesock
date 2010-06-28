
module ScanDB (
    -- Types
    Scan(Scan),
    DB(),

    -- Accessors
    scanId,
    scanTime,
    scanBitmap,

    -- Operations
    getDefaultDBPath,
    initDB,
    storeScan,
    lookupScan,

    -- Testing
    withTestDb
    ) where

import Control.Exception
import Control.Monad
import Data.Char (isSpace)
import Data.UUID
import Data.Bitmap
import Data.Bitmap.IO
import Data.Time.Clock
import Data.Time.Format
import ImageUtil (toGD, fromGD, makeSampleBitmap, assertBitmapsSimilar)
import System.Directory
import System.Locale
import System.Random
import Test.HUnit
import Test.QuickCheck
import qualified Graphics.GD as GD

data Scan = Scan { scanId :: UUID
                 , scanTime :: UTCTime
                 , scanBitmap :: Bitmap Word8
                 }

scanImagePath :: DB -> UUID -> FilePath
scanImagePath db id =
  dbScanPath db ++ "/" ++ show id ++ ".png"

scanAnnotationsPath :: DB -> UUID -> FilePath
scanAnnotationsPath db id =
  dbScanPath db ++ "/" ++ show id ++ ".yaml"

data DB = DB { dbPath :: FilePath
             } deriving (Show, Eq)

dbScanPath :: DB -> FilePath
dbScanPath db =
  dbPath db ++ "/scans"

-- | Find the default scan database (in the user's home directory)
getDefaultDBPath :: IO FilePath
getDefaultDBPath = getAppUserDataDirectory "OneSock"

-- | Initialize the scan database and prepare it for use
initDB :: FilePath -> IO DB
initDB p = 
  let
    maybeMkdir d = do
      isDir <- doesDirectoryExist d
      unless isDir $ createDirectory d
  in do
    maybeMkdir p
    maybeMkdir $ p ++ "/scans"
    return DB{dbPath=p}

-- | Store a scan into a scan database
storeScan :: DB -> Scan -> IO ()
storeScan db scan = do
  img <- toGD $ scanBitmap scan
  GD.savePngFile (scanImagePath db $ scanId scan) img
  writeFile (scanAnnotationsPath db $ scanId scan) $ toYaml $ scanAnnotations scan

test_storeScanWritesFile =
  "storeScan writes files" ~: TestCase $
      withTestDb $ \db -> do
        id <- randomIO :: IO UUID
        sampleBitmap <- makeSampleBitmap
        now <- getCurrentTime
        let scan = Scan{scanId = id, scanTime = now, scanBitmap = sampleBitmap}
        storeScan db scan
        let pngFname = dbPath db ++ "/scans/" ++ show id ++ ".png"
        doesFileExist pngFname >>= assertBool "image file does not exist"
        let yamlFname = dbPath db ++ "/scans/" ++ show id ++ ".yaml"
        doesFileExist yamlFname >>= assertBool "yaml file does not exist"

-- | Retrieve a scan from a scan database
lookupScan :: DB -> UUID -> IO Scan
lookupScan db id = do
  bitmap <- GD.loadPngFile (scanImagePath db id) >>= fromGD
  annotations <- fromYaml `fmap` readFile (scanAnnotationsPath db id)
  return Scan{scanId=id, scanTime = annTime annotations, scanBitmap=bitmap}

test_lookupScanReturnsWhatWasStored =
  "lookupScan returns what was stored" ~: TestCase $
    withTestDb $ \db -> do
      id <- randomIO :: IO UUID
      sampleBitmap <- makeSampleBitmap
      now <- getCurrentTime
      let scan = Scan{scanId = id, scanTime = addUTCTime (-47) now, scanBitmap = sampleBitmap}
      storeScan db scan
      scan' <- lookupScan db id
      assertEqual "scanId scan'" (scanId scan) (scanId scan')
      assertEqual "scanTime scan'" (scanTime scan) (scanTime scan')
      assertBitmapsSimilar (scanBitmap scan) (scanBitmap scan')

--
-- Scan annotations
--

data Annotations
  = Annotations { annTime :: UTCTime }
  deriving (Eq, Show)

timeFormat = "%Y-%m-%d %X%Q"

instance Arbitrary Annotations where
  arbitrary = do
    n <- choose (-1000,1000)
    let time = readTime defaultTimeLocale timeFormat "2010-06-27 16:42:18.4322"
    return Annotations{ annTime = addUTCTime (fromInteger n) time }
    
prop_annotationsRoundTripToYaml a =
  (fromYaml . toYaml) a == a

toYaml :: Annotations -> String
toYaml a =
  "scanTime: " ++ formatTime defaultTimeLocale timeFormat (annTime a) ++ "\n"

fromYaml :: String -> Annotations
fromYaml s =
  let
    l = head $ lines s
    timeStr = dropWhile isSpace $ tail $ dropWhile (/= ':') l
    time = readTime defaultTimeLocale timeFormat timeStr
  in
    Annotations{ annTime = time }

scanAnnotations scan =
  Annotations{ annTime = scanTime scan }

-- | Create a temporary db for the duration of an IO action
withTestDb :: (DB -> IO a) -> IO ()
withTestDb action = do
  tmpDir <- getTemporaryDirectory
  let
    testDbPath = tmpDir ++ "/onesockTestDb"
  finally (removeDirectoryRecursive testDbPath) $
    initDB testDbPath >>= action

