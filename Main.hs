{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL hiding (Point)
import GHC.Word
import Control.Concurrent
import Foreign.C.Types

-- SDL_ALPHA_TRANSPARENT
transparent :: Word8
transparent = 0

-- SDL_ALPHA_OPAQUE
opaque :: Word8
opaque = 255

-- sleeps for a determined amount of seconds
sleep :: Int -> IO ()
sleep = threadDelay . (*1000000)

-- An alias for a 2 dimension vector
type Point = V2 CInt

-- Draw a triangle using vertex p, q and r.
-- Lines are drawn from p->q->r->p (p to q, then q to r, finally closing r to p)
drawTriangle :: Renderer -> Point -> Point -> Point -> IO ()
drawTriangle renderer p q r = do
  SDL.drawLine renderer (P p) (P q)
  SDL.drawLine renderer (P q) (P r)
  SDL.drawLine renderer (P r) (P p)

main :: IO ()
main = do
  SDL.initializeAll
  window   <- SDL.createWindow   "STL Viewer" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1)  SDL.defaultRenderer
  SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 opaque    -- set   renderer color to black
  SDL.clear             renderer                           -- clear renderer using color
  SDL.rendererDrawColor renderer $= V4 255 255 255 opaque  -- set   renderer color to white
  drawTriangle          renderer (V2 0 480) (V2 320 0) (V2 640 480)
  SDL.present           renderer
  SDL.showWindow        window
  sleep 10
  SDL.destroyWindow window
  SDL.quit
