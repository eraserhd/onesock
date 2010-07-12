
module CachingImageDB (

  CachingImageDB,
  openCachingImageDB,

  ImageSizeType,

  storeImage,
  lookupImage,
  
  ) where

import Control.Exception (bracket, IOException)
import Control.Monad (when)
import Data.Bitmap
import qualified Graphics.GD as GD
import qualified Data.Cache.LRU.IO as LRU
import Data.UUID
import ImageUtil (makeSampleBitmap, toGD, fromGD, assertBitmapsSimilar)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive, doesFileExist, removeFile, createDirectoryIfMissing)
import System.Random (randomIO)
import qualified Test.HUnit as T

data ImageSizeType
  = FullSize | Scaled Int
    deriving (Eq, Ord)

data CachingImageDB
  = CachingImageDB { dbPath :: FilePath
                   , dbFullSizeLru :: LRU.AtomicLRU UUID (Bitmap Word8)
                   , dbScaledLru :: LRU.AtomicLRU (UUID, Int) (Bitmap Word8)
                   }

maximumCachedFullSizeImages = 5

maximumCachedScaledImages = 45

openCachingImageDB :: FilePath -> IO CachingImageDB
openCachingImageDB path = do
  fsLru <- LRU.newAtomicLRU $ Just maximumCachedFullSizeImages
  scaledLru <- LRU.newAtomicLRU $ Just maximumCachedScaledImages
  return CachingImageDB{ dbPath = path
                       , dbFullSizeLru = fsLru
                       , dbScaledLru = scaledLru
                       }

storeImage :: CachingImageDB -> UUID -> Bitmap Word8 -> IO ()
storeImage db id bmap = do
  let path = dbPngFilePath db id
  exists <- doesFileExist path
  when exists $ ioError $ userError "Image already exists in image database"
  toGD bmap >>= GD.savePngFile path

lookupImage :: CachingImageDB -> UUID -> ImageSizeType -> IO (Bitmap Word8)
lookupImage db id FullSize = do
  maybeCached <- LRU.lookup id $ dbFullSizeLru db
  case maybeCached of
    Just bmap -> return bmap
    Nothing   -> do
      bmap <- GD.loadPngFile (dbPngFilePath db id) >>= fromGD
      LRU.insert id bmap $ dbFullSizeLru db
      return bmap
lookupImage db id (Scaled s) = do
  maybeCached <- LRU.lookup (id, s) $ dbScaledLru db
  case maybeCached of
    Just bmap -> return bmap
    Nothing   -> do
      fullBmap <- lookupImage db id FullSize
      let scaledBmap = scaleImage fullBmap s
      LRU.insert (id,s) scaledBmap $ dbScaledLru db
      return scaledBmap

--
-- Local utilities
--

dbPngFilePath :: CachingImageDB -> UUID -> FilePath
dbPngFilePath db id =
  dbPath db ++ "/" ++ show id ++ ".png"

withTestDb :: (CachingImageDB -> IO a) -> IO a
withTestDb =
  let
    openTmpDb = do
      tmpDir <- getTemporaryDirectory
      let testDbPath = tmpDir ++ "/testImageDB"
      createDirectoryIfMissing False testDbPath
      openCachingImageDB testDbPath

    rmTmpDb = removeDirectoryRecursive . dbPath
  in
    bracket openTmpDb rmTmpDb

test_storeImageWritesPngFile =
  withTestDb $ \db -> do
    bitmap <- makeSampleBitmap
    id <- randomIO :: IO UUID
    storeImage db id bitmap
    pngExists <- doesFileExist $ dbPngFilePath db id
    T.assertBool "png exists" pngExists

test_storeImageThrowsExceptionIfFileExists =
  withTestDb $ \db -> do
    bitmap <- makeSampleBitmap
    id <- randomIO :: IO UUID
    storeImage db id bitmap
    catch (do
      storeImage db id bitmap
      T.assertBool "did not throw" False)
      (\e -> return ())

scaleImage :: Bitmap Word8 -> Int -> Bitmap Word8
scaleImage bmap n =
  let
    (w,h) = bitmapSize bmap

    larger :: Integer
    larger = fromIntegral $ max w h

    newSize :: (Int, Int)
    newSize = ( fromIntegral (fromIntegral w * fromIntegral n `div` larger),
                fromIntegral (fromIntegral h * fromIntegral n `div` larger) )
  in
    bilinearResample bmap newSize (Just $ bitmapRowAlignment bmap)

test_lookupImageCanGetFullSizeImage =
  withTestDb $ \db -> do
    a <- makeSampleBitmap
    id <- randomIO :: IO UUID
    storeImage db id a
    b <- lookupImage db id FullSize
    assertBitmapsSimilar a b

test_lookupImageThrowsWhenFileNotThere =
  withTestDb $ \db -> do
    a <- makeSampleBitmap
    id <- randomIO :: IO UUID
    storeImage db id a
    removeFile $ dbPngFilePath db id
    catch (do
      lookupImage db id FullSize
      T.assertBool "did not throw" False)
      (\e -> return ())

test_lookupImageCanGetFullSizeImageFromCache = 
  withTestDb $ \db -> do
    a <- makeSampleBitmap
    id <- randomIO :: IO UUID
    storeImage db id a
    lookupImage db id FullSize -- puts in cache
    removeFile $ dbPngFilePath db id
    b <- lookupImage db id FullSize
    assertBitmapsSimilar a b

test_lookupImageCanGetScaledImage =
  withTestDb $ \db -> do
    a <- makeSampleBitmap
    id <- randomIO :: IO UUID
    storeImage db id a
    b <- lookupImage db id $ Scaled 23
    T.assertEqual "bitmapSize b" (23, 23) $ bitmapSize b

test_lookupImageCanGetScaledImageFromCache =
  withTestDb $ \db -> do
    a <- makeSampleBitmap
    id <- randomIO :: IO UUID
    storeImage db id a
    b <- lookupImage db id $ Scaled 23
    T.assertEqual "bitmapSize b" (23, 23) $ bitmapSize b
    LRU.delete id $ dbFullSizeLru db
    removeFile $ dbPngFilePath db id
    c <- lookupImage db id $ Scaled 23
    assertBitmapsSimilar b c

