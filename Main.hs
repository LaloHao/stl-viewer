{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import GHC.Word
import Control.Concurrent

-- SDL_ALPHA_TRANSPARENT
transparent :: Word8
transparent = 0

-- SDL_ALPHA_OPAQUE
opaque :: Word8
opaque = 255

-- sleeps for a determined amount of seconds
sleep :: Int -> IO ()
sleep = threadDelay . (*1000000)

main :: IO ()
main = do
  SDL.initializeAll
  window   <- SDL.createWindow   "STL Viewer" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1)  SDL.defaultRenderer
  SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 opaque    -- set   renderer color to black
  SDL.clear             renderer                           -- clear renderer using color
  SDL.rendererDrawColor renderer $= V4 255 255 255 opaque  -- set   renderer color to white
  SDL.drawLine          renderer (P $ V2 0   480) (P $ V2 320   0)  -- draw a line from <0  , 480> to <320,   0>
  SDL.drawLine          renderer (P $ V2 640 480) (P $ V2 320   0)  -- draw a line from <640, 480> to <320,   0>
  SDL.drawLine          renderer (P $ V2 0   480) (P $ V2 640 480)  -- draw a line from <0  , 480> to <640, 480>
  SDL.present           renderer
  SDL.showWindow        window
  sleep 10
  SDL.destroyWindow window
  SDL.quit
