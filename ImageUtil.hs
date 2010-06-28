
module ImageUtil (
  toGD,
  fromGD,

  -- Testing
  makeSampleBitmap,
  assertBitmapsSimilar,
  withTestBitmapFile
  ) where

import Data.Bitmap
import Data.Bitmap.IO
import Data.UUID
import Control.Exception (bracket)
import Control.Monad (foldM, when)
import qualified Graphics.GD as GD
import System.Random (randomIO)
import System.Directory (getTemporaryDirectory, removeFile)
import qualified Test.HUnit as T (assertBool, assertEqual)

-- | Convert a bitmap to a GD image
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

-- | Convert a GD image to a bitmap
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

makeSampleBitmap :: IO (Bitmap Word8)
makeSampleBitmap = do
  bitmap <- newBitmap (45, 45) 3 Nothing
  mapM_ (\i -> unsafeWritePixel3 bitmap (i,i) (255,127,10)) [0..44]
  return bitmap

withTestBitmapFile :: (FilePath -> IO a) -> IO a
withTestBitmapFile =
  bracket newTestBitmapFile removeFile
  where
    newTestBitmapFile :: IO FilePath
    newTestBitmapFile = do
      tmpDir <- getTemporaryDirectory
      id <- randomIO :: IO UUID
      let pngFile = tmpDir ++ "/" ++ show id ++ ".png"
      bmap <- makeSampleBitmap
      toGD bmap >>= GD.savePngFile pngFile
      return pngFile

assertBitmapsSimilar a b = do
  let
    (awidth, aheight) = bitmapSize a
    (bwidth, bheight) = bitmapSize b
  T.assertEqual "bwidth" awidth bwidth
  T.assertEqual "bheight" aheight bheight

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
  T.assertBool "bitmap error is too high" $ err < 2.0

