
module Onesock.DB.Scans (
    -- Types
    Scan,
    DB(),

    -- Operations
    initScanDB,
    storeScan,
    lookupScan,

    tests
    ) where

import Test.HUnit
import System.Random
import System.Directory
import Data.UUID
import Data.Bitmap
import Data.Bitmap.IO
import Control.Exception
import Control.Monad
import qualified Graphics.GD as GD

data Scan = Scan { scanId :: UUID
                 , scanBitmap :: Bitmap Word8
                 }

scanImagePath :: DB -> UUID -> FilePath
scanImagePath db id =
  dbScanPath db ++ "/" ++ show id ++ ".png"

data DB = DB { dbPath :: FilePath
             } deriving (Show, Eq)

dbScanPath :: DB -> FilePath
dbScanPath db =
  dbPath db ++ "/scans"

toGD :: Bitmap Word8 -> IO GD.Image
toGD bitmap = do
  when (bitmapNChannels bitmap /= 3) $ error "Wrong number of channels (must have 3)"
  let
    (width, height) = bitmapSize bitmap
    allCoords = [ (x,y) | x <- [0..(width-1)], y <- [0..(height-1)] ]
  img <- GD.newImage (width,height)
  mapM_ (\(x,y) -> do
    (r,g,b) <- unsafeReadPixel3 bitmap (x,y)
    GD.setPixel (x,y) (GD.rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)) img
    ) allCoords
  return img

fromGD :: GD.Image -> IO (Bitmap Word8)
fromGD img = do
  (width, height) <- GD.imageSize img
  let allCoords = [ (x,y) | x <- [0..(width-1)], y <- [0..(height-1)] ]
  bitmap <- newBitmap (width, height) 3 Nothing
  mapM_ (\(x,y) -> do
    pel <- GD.getPixel (x,y) img
    unsafeWritePixel3 bitmap (x,y) (fromIntegral $ GD.red pel :: Word8,
                                    fromIntegral $ GD.green pel :: Word8,
                                    fromIntegral $ GD.blue pel :: Word8)
    ) allCoords
  return bitmap

-- | Initialize the scan database and prepare it for use
initScanDB :: FilePath -> IO DB
initScanDB p = 
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

-- | Retrieve a scan from a scan database
lookupScan :: DB -> UUID -> IO Scan
lookupScan db id = do
  bitmap <- GD.loadPngFile (scanImagePath db id) >>= fromGD
  return Scan{scanId=id, scanBitmap=bitmap}

--
-- Tests
--

makeSampleBitmap = do
  bitmap <- newBitmap (45, 45) 3 Nothing
  mapM_ (\i -> unsafeWritePixel3 bitmap (i,i) (255,127,10)) [0..44]
  return bitmap

-- For testing: Make sure two bitmaps are similar and have the same dimensions.
assertBitmapsSimilar a b = do
  let
    (awidth, aheight) = bitmapSize a
    (bwidth, bheight) = bitmapSize b
  assertEqual "bwidth" awidth bwidth
  assertEqual "bheight" aheight bheight

  sum <- foldM (\acc (x,y) -> do
    (ra,ga,ba) <- unsafeReadPixel3 a (x,y)
    (rb,gb,bb) <- unsafeReadPixel3 b (x,y)
    let rd = fromIntegral $ ra - rb :: Int
    let gd = fromIntegral $ ga - gb :: Int
    let bd = fromIntegral $ ba - bb :: Int
    return $ acc + rd*rd + gd*gd + bd*bd
    ) 0 [ (x,y) | x <- [0..(awidth-1)], y <- [0..(aheight-1)] ]

  let err = sqrt $ (fromIntegral sum :: Double) /
                   (fromIntegral (3*awidth*aheight) :: Double)
  assertBool "bitmap error is too high" $ err < 2.0

-- For testing: set up and tear down a test database on the filesystem around
-- an I/O action.
withTestDb :: (DB -> IO a) -> IO ()
withTestDb action = do
  tmpDir <- getTemporaryDirectory
  let
    testDbPath = tmpDir ++ "/onesockTestDb"
  finally (removeDirectoryRecursive testDbPath) $
    initScanDB testDbPath >>= action

tests =
  TestLabel "Onesock.DB.Scans suite" $ TestList [
    "storeScan writes file" ~: TestCase $
      withTestDb $ \db -> do
        id <- randomIO :: IO UUID
        sampleBitmap <- makeSampleBitmap
        let scan = Scan{scanId = id, scanBitmap = sampleBitmap}
        storeScan db scan
        let fname = dbPath db ++ "/scans/" ++ show id ++ ".png"
        doesFileExist fname >>= assertBool "image file does not exist"
  , "lookupScan returns what was stored" ~: TestCase $
      withTestDb $ \db -> do
        id <- randomIO :: IO UUID
        sampleBitmap <- makeSampleBitmap
        let scan = Scan{scanId = id, scanBitmap = sampleBitmap}
        storeScan db scan
        scan' <- lookupScan db id
        assertEqual "scanId scan'" (scanId scan) (scanId scan')
        assertBitmapsSimilar (scanBitmap scan) (scanBitmap scan')
  ]
