{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where
import Prelude hiding (writeFile)
import STL
import STL.Parser hiding (Vector)
import System.Exit (exitFailure, exitSuccess)
import Graphics.UI.GLUT hiding (Solid, Normal, Help, color, Color)
import Data.IORef
import System.Random
import Control.Monad (replicateM, void)
import Data.Maybe
import Text.Read (readMaybe)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0)
import Data.Word
import Foreign.ForeignPtr
import Codec.Picture
import Text.Printf
import Options
import Control.Lens
import GHC.Int

getFacets :: String -> IO ([Facet], Int)
getFacets file = do
  model <- stlParseFromFile file
  case model of
    Left err -> do
      putStrLn $ "Error parsing file \"" <> file <> "\""
      putStrLn $ show err
      exitFailure
    _ -> return ()
  let (Right (Solid _ facets)) = model
  let !nfacets = length facets
  return (facets, nfacets)

main :: IO ()
main = do
  (_     , argv)   <- getArgsAndInitialize
  (config, [file]) <- arguments argv
  (facets, n)      <- getFacets file

  angle  <- newIORef 0
  frames <- newIORef (0 :: Int)
  colors <- replicateM n randomColor

  let w, h :: Int32
      w = config ^. optWidth
      h = config ^. optHeight
  putStrLn $ "Rendering with initial geometry w=" <> show w <> ", h=" <> show h
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size w h

  window <- createWindow "STL Viewer"

  reshapeCallback $= Just reshape
  depthFunc       $= Just Less
  idleCallback    $= Just (idle angle)
  displayCallback $= display window angle colors facets config frames
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: Window -> IORef GLfloat -> [IO ()] -> [Facet] -> Options -> IORef Int -> DisplayCallback
display window angle colors facets config frameCount = do
  clear [ ColorBuffer, DepthBuffer ]
  clear [ ColorBuffer ]
  loadIdentity
  θ <- get angle
  rotate θ $ Vector3 rx 0 0
  rotate θ $ Vector3 0 ry 0
  rotate θ $ Vector3 0  0 rz
  scale κ κ κ
  mapM_ renderFacet (zipWith (,) facets colors)
  swapBuffers
  frames <- get frameCount
  saveFrame window config frames
  frameCount $~! (+1)
  where s = config ^. optScale
        rx = config ^. optRotateX
        ry = config ^. optRotateY
        rz = config ^. optRotateZ
        κ = 1 / s

saveFrame :: Window -> Options -> Int -> IO ()
saveFrame window config frameCount
  | output == False = return ()
  | otherwise = do
  image <- getFrame window
  if frameCount >= maxFrames
    then do
      putStrLn "Done"
      exitSuccess
    else do
      let frame = printf format frameCount
      let file = (dir ++ "/" ++ frame)
      putStrLn $ "Writing " ++ file
      writePng file image
  where (Just maxFrames) = config ^. optOutputFrames
        (Just dir)       = config ^. optOutputFolder
        (Just format)    = config ^. optOutputFormat
        output = (outputFrames && outputFolder && outputFormat)
        outputFrames = isJust (config ^. optOutputFrames)
        outputFolder = isJust (config ^. optOutputFolder)
        outputFormat = isJust (config ^. optOutputFormat)

renderFacet :: (Facet, IO ()) -> IO ()
renderFacet ((Facet (Normal [nx, ny, nz]) (Vertices vertices)), color)
  = preservingMatrix $ do
  normal3f nx ny nz
  void $ color
  drawTriangle v0 v1 v2
  where 
    v0 = vertex3f x0 y0 z0
    v1 = vertex3f x1 y1 z1
    v2 = vertex3f x2 y2 z2
    [ x0, y0, z0 ,
      x1, y1, z1 ,
      x2, y2, z2 ] = vertices :: [ GLfloat ]
renderFacet ((Facet (Normal n) (Vertices vs)), _) = do
  putStrLn $ "A problem has ocurred on facet with normal vector " <> (unwords $ show <$> n)
  -- mapM_ putStrLn $ unwords <$> show vs
  return ()

type Frame = Image PixelRGBA8

idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+1)
  postRedisplay Nothing

getFrame :: Window -> IO Frame
getFrame window = do
  currentWindow $= Just window
  Size w h <- get windowSize
  let npixels = fromIntegral (w*h) :: Int
      nbytes  = 4*npixels
  fptr <- mallocForeignPtrArray nbytes :: IO (ForeignPtr Word8)
  withForeignPtr fptr $ \ptr -> do
    let pdata = PixelData RGBA UnsignedByte ptr :: PixelData Word8
    readPixels (Position 0 0) (Size w h) pdata
  let fptr' = castForeignPtr fptr :: ForeignPtr (PixelBaseComponent PixelRGBA8)
  let imgdata = unsafeFromForeignPtr0 fptr' npixels :: Vector (PixelBaseComponent PixelRGBA8)
  let image = Image (fromIntegral w) (fromIntegral h) imgdata :: Image PixelRGBA8
  return image

randomFloat :: IO GLfloat
randomFloat = getStdRandom $ randomR (0.0, 1.0)

randomColor :: IO (IO ())
randomColor = do
  [r, g, b] <- replicateM 3 randomFloat
  return $ color3f r g b
