
module GUI where

import Test.QuickCheck
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import ScanDB
import Data.UUID
import qualified Data.Bitmap.IO as BM
import Foreign.Ptr
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.Bitmap

runGUI = do
  getArgsAndInitialize
  db <- getDefaultDBPath >>= initDB
  let (Just uuid) = fromString "b4432054-27de-4e11-9e88-b2364e28cce2"
  scan <- lookupScan db uuid
  initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
  createWindow "OneSock"
  myInit scan
  displayCallback $= display scan
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboard
  mainLoop

myInit scan = do
  shadeModel $= Flat
  depthFunc $= Just Less
  [texName] <- genObjectNames 1
  textureBinding Texture2D $= Just texName
  textureFunction $= Modulate
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)

  BM.withBitmap (scanBitmap scan) $ \(w,h) _ _ p -> do
    putStrLn $ show (w,h)
    let
      textureSize = (TextureSize2D (fromIntegral w) (fromIntegral h))
      pixelData = PixelData RGB UnsignedByte (castPtr p)
    texImage2D Nothing NoProxy 0 RGB' textureSize 0 pixelData

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

display :: Scan -> DisplayCallback
display scan = do
  clear [ColorBuffer,DepthBuffer]
  color (Color3 1 1 1 :: Color3 GLfloat)
  texture Texture2D $= Enabled
  renderPrimitive Quads $ do
    let (a,b,c,d) = unitVerticesWithSameAspect $ bitmapSize $ scanBitmap scan
    texCoord (TexCoord2 0 1 :: TexCoord2 GLfloat); vertex a
    texCoord (TexCoord2 1 1 :: TexCoord2 GLfloat); vertex b
    texCoord (TexCoord2 1 0 :: TexCoord2 GLfloat); vertex c
    texCoord (TexCoord2 0 0 :: TexCoord2 GLfloat); vertex d
  texture Texture2D $= Disabled
  flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-4.0) 4.0 (-4.0*hf/wf) (4.0*hf/wf) (-4.0) 4.0
      else ortho (-4.0*wf/hf) (4.0*wf/hf) (-4.0) 4.0 (-4.0) 4.0
   matrixMode $= Modelview 0
   loadIdentity

unitVerticesWithSameAspect :: (Int, Int) -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
unitVerticesWithSameAspect (w,h) = 
  let
    wF = fromIntegral w :: GLfloat
    hF = fromIntegral h :: GLfloat
    wShrink :: GLfloat
    wShrink = if w < h
              then (1.0 - wF/hF) / 2.0
              else 0

    hShrink :: GLfloat
    hShrink = if w < h
              then 0
              else (1.0 - hF/wF) / 2.0

    left = wShrink
    right = 1.0 - wShrink
    top = hShrink
    bottom = 1.0 - hShrink
  in
    (Vertex3 left top 0,
     Vertex3 right top 0,
     Vertex3 right bottom 0,
     Vertex3 left bottom 0)

check = do
  quickCheck (\(w,h) -> let (Vertex3 _ _ a, Vertex3 _ _ b, Vertex3 _ _ c, Vertex3 _ _ d) = unitVerticesWithSameAspect (w,h)
                        in (a == 0) && (b == 0) && (c == 0) && (d == 0))
  quickCheck (\(w,h) -> let (_, _, Vertex3 _ a _, Vertex3 _ b _) = unitVerticesWithSameAspect (w,h)
                        in a == b)
