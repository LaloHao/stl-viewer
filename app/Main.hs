{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import STL
import qualified SDL
import System.Exit (exitFailure)
import System.Environment
import Control.Concurrent
import Graphics.UI.GLUT
import Data.IORef
import System.Random
import Control.Monad (replicateM)
import System.IO.Unsafe

main :: IO ()
main = do
  (program, arguments) <- getArgsAndInitialize
  window <- createWindow "STL Viewer"
  angle <- newIORef 0
  colors <- replicateM 2 randomColor
  displayCallback $= display angle colors
  idleCallback    $= Just (idle angle)
  reshapeCallback $= Just reshape
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: IORef GLfloat -> [IO ()] -> DisplayCallback
display angle colors = do
  clear [ ColorBuffer, DepthBuffer ]
  clear [ ColorBuffer ]
  loadIdentity
  θ <- get angle
  rotate θ $ Vector3 0 1 1
  drawTriangle color₁ v₀ v₁ v₂
  drawTriangle color₂ v₀ v₃ v₄
  flush
  where
    w  = (0.5 :: GLfloat)
    v₀ = (vertex3f (-w) (-w) (-w))
    v₁ = (vertex3f ( w) (-w) (-w))
    v₂ = (vertex3f ( w) ( w) (-w))
    v₃ = (vertex3f (-w) ( w) ( w))
    v₄ = (vertex3f ( w) ( w) ( w))
    [color₁, color₂] = colors

idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+ 0.1)
  postRedisplay Nothing

randomFloat :: IO GLfloat
randomFloat = getStdRandom $ randomR (0.0, 1.0)

randomColor :: IO (IO ())
randomColor = do
  [r, g, b] <- replicateM 3 randomFloat
  return $ color3f r g b
