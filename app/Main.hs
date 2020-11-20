{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import STL
import qualified SDL
import System.Exit (exitFailure)
import System.Environment
import Control.Concurrent

main :: IO ()
main = do
  SDL.initializeAll
  window   <- SDL.createWindow   "STL Viewer" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1)  SDL.defaultRenderer
  SDL.rendererDrawColor renderer $= V4 0 0 0 opaque    -- set   renderer color to black
  SDL.clear             renderer                           -- clear renderer using color
  SDL.rendererDrawColor renderer $= V4 255 255 255 opaque  -- set   renderer color to white
  drawTriangle          renderer (V2 0 480) (V2 320 0) (V2 640 480)
  SDL.present           renderer
  SDL.showWindow        window
  sleep 10
  SDL.destroyWindow window
  SDL.quit
