
module ImageUtil (
  toGD,
  fromGD
  ) where

import Data.Bitmap
import Data.Bitmap.IO
import Control.Monad
import qualified Graphics.GD as GD

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

